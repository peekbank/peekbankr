# peekbankr reads data from the versioned peekbank dataset hosted on Redivis
# (https://redivis.com/datapages/datasets/peekbank). This file contains the
# machinery for resolving Peekbank releases to Redivis dataset versions and
# for fetching tables and running queries against them.

# cached reads, shared by every client for the life of the session
.peekbankr_env <- new.env(parent = emptyenv())

peekbank_organization <- "datapages"
peekbank_dataset_reference <- "peekbank:a3v0"

# check that the redivis package is available; if not, message and FALSE
redivis_available <- function() {
  if (!requireNamespace("redivis", quietly = TRUE)) {
    message(
      "peekbankr needs the `redivis` package to access Peekbank data.\n",
      "Install it with:\n",
      '  install.packages("redivis", repos = "https://langcog.r-universe.dev")')
    return(FALSE)
  }
  TRUE
}


# muffle the redivis client's advisory warning about unqualified references
# (we use table names instead if ids, but because they are constant in our project, this warning does not apply to us)
quiet_redivis <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("No reference id was provided", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# A wrapper that converts network failures into NULL  to meet CRAN requirements
pb_try <- function(expr) {
  tries <- 3
  # substitute() because a promise would only ever evaluate once (due to R lazy eval)
  expr <- substitute(expr)
  env <- parent.frame()
  for (i in seq_len(tries)) {
    result <- tryCatch(quiet_redivis(eval(expr, env)), error = function(e) {
      # Redivis rejected the request itself, so sending it again cannot help.
      # 408 and 429 are the two it asks you to repeat.
      if (inherits(e, "redivis_api_error") &&
            isTRUE(e$status_code < 500) &&
            !e$status_code %in% c(408, 429)) {
        stop(conditionMessage(e), call. = FALSE)
      }
      if (i < tries) {
        message("Redivis request failed (attempt ", i, "/", tries,
                "), retrying...")
        Sys.sleep(2 ^ i)
      } else {
        message("Could not retrieve data from Redivis. Please check your ",
                "internet connection. If this error persists please contact ",
                "peekbank-dev@lists.stanford.edu.\n(", conditionMessage(e), ")")
      }
      NULL
    })
    if (!is.null(result)) return(result)
  }
  NULL
}

# reference to the peekbank Redivis dataset, optionally at a version tag
pb_dataset <- function(redivis_tag = NULL) {
  redivis::redivis$organization(peekbank_organization)$dataset(
    peekbank_dataset_reference, version = redivis_tag)
}

published_redivis_tags <- function() {
  if (!is.null(.peekbankr_env$published_redivis_tags)) {
    return(.peekbankr_env$published_redivis_tags)
  }
  if (!redivis_available()) return(NULL)
  props <- pb_try(pb_dataset()$get()$properties)
  if (is.null(props)) return(NULL)
  versions <- pb_try(pb_dataset()$list_versions())
  if (is.null(versions)) return(NULL)
  # an editor's credentials also see the dataset's unreleased "next" version
  released <- Filter(function(v) isTRUE(v$properties$isReleased), versions)
  if (length(released) == 0) return(NULL)
  index <- vapply(released, function(v) v$properties$index, numeric(1))
  redivis_tags <- vapply(released, function(v) v$properties$tag, character(1))
  .peekbankr_env$published_redivis_tags <- list(
    current_redivis_tag = props$version$tag,
    redivis_tags = unname(redivis_tags[order(index, decreasing = TRUE)]))
  .peekbankr_env$published_redivis_tags
}

# read the release_info table at a given version tag -> return release name string
release_name_at <- function(redivis_tag) {
  key <- paste0("release_name_", redivis_tag)
  if (is.null(.peekbankr_env[[key]])) {
    info <- pb_try(pb_dataset(redivis_tag)$table("release_info")$to_tibble())
    if (is.null(info) || nrow(info) == 0) return(NULL)
    .peekbankr_env[[key]] <- info$release_name[[1]]
  }
  .peekbankr_env[[key]]
}

# a release name, or "current", to the release and the Redivis version tag
# holding it
resolve_release <- function(db_version = "current") {
  idx <- published_redivis_tags()
  if (is.null(idx)) {
    stop("Could not reach the peekbank dataset on Redivis to resolve ",
         "versions.", call. = FALSE)
  }
  if (identical(db_version, "current")) {
    # the tag is known and the release name is not, so read it: the caller
    # did not name a release and still has to be told which one they got
    redivis_tag <- idx$current_redivis_tag
    release <- release_name_at(redivis_tag)
  } else {
    # the release name is known and the tag is not, so search for it,
    # newest first and stopping at the match
    release <- db_version
    redivis_tag <- NULL
    for (candidate in idx$redivis_tags) {
      if (identical(release_name_at(candidate), release)) {
        redivis_tag <- candidate
        break
      }
    }
    if (is.null(redivis_tag)) {
      stop("Version '", db_version, "' not found. Available versions: ",
           "'current', ",
           paste(sprintf("'%s'", peekbank_releases()$release),
                 collapse = ", "), ".", call. = FALSE)
    }
  }
  message("Using database version: '", release,
          "' (Redivis version ", redivis_tag, ").")
  list(release = release, redivis_tag = redivis_tag)
}

# fetch a whole table as a tibble, cached per session + version
pb_table <- function(connection, name) {
  redivis_tag <- connection$redivis_tag
  key <- paste(redivis_tag, name)
  if (is.null(.peekbankr_env[[key]])) {
    if (!redivis_available()) return(NULL)
    .peekbankr_env[[key]] <- pb_try(
      pb_dataset(redivis_tag)$table(name)$to_tibble())
  }
  .peekbankr_env[[key]]
}

# run a SQL query against the dataset version (server-side filtering for the
# big timepoint tables), cached per session + version; BigQuery Standard SQL
# dialect
pb_query <- function(connection, sql) {
  redivis_tag <- connection$redivis_tag
  key <- paste(redivis_tag, sql)
  if (is.null(.peekbankr_env[[key]])) {
    if (!redivis_available()) return(NULL)
    .peekbankr_env[[key]] <- pb_try(
      pb_dataset(redivis_tag)$query(sql)$to_tibble())
  }
  .peekbankr_env[[key]]
}

#' @export
print.peekbank_connection <- function(x, ...) {
  cat("<peekbank_connection> release ", x$release,
      " (Redivis ", peekbank_organization, "/", peekbank_dataset_reference,
      " version ", x$redivis_tag, ")\n", sep = "")
  invisible(x)
}

# ---- data files (raw_data / processed_data / READMEs) -----------------------
# Peekbank's files live in the companion datapages.peekbank_files dataset on
# Redivis: a single file-index table `files` whose file names are relative
# paths (<dataset>/raw_data/..., <dataset>/README.md).

peekbank_files_reference <- "peekbank_files:frvk"

pb_files_dataset <- function() {
  redivis::redivis$organization(peekbank_organization)$dataset(
    peekbank_files_reference)
}

# tibble of (file_id, file_name, size) for files under a name prefix
pb_files_query <- function(prefix) {
  if (!redivis_available()) return(NULL)
  sql <- sprintf(
    "SELECT file_id, file_name, size FROM files WHERE STARTS_WITH(file_name, '%s')",
    gsub("'", "\\\\'", prefix))
  pb_try(pb_files_dataset()$query(sql)$to_tibble())
}

# Fetch one file from the peekbank_files dataset, given a row of a
# pb_files_query() result.
#
# Workaround for a redivis bug, observed in 0.12.12: the public helpers
# ($file(), list_files(), to_directory()) all build a directory tree through
# add_directory_file(), which pastes a directory name straight into a regex:
#
#   sub(paste0("^", self_path, "/?"), "", file_path, fixed = FALSE)
#
# A name holding a regex metacharacter therefore fails to strip its own prefix,
# and the function recurses on an ever-longer path until the pattern will not
# compile. Building the file from its id instead sidesteps the tree walk.
#
# When that is fixed upstream, this function becomes:
#   pb_files_dataset()$table("files")$file(file_name)$download(
#     path = dest, overwrite = TRUE, progress = FALSE)
pb_fetch_file <- function(file_id, file_name, size, dest) {
  redivis_file <- utils::getFromNamespace("File", "redivis")
  redivis_file$new(id = file_id, name = file_name,
                   properties = list(size = size))$download(
                     path = dest, overwrite = TRUE, progress = FALSE)
}

# download rows of a pb_files_query() result, recreating the relative paths
# under local_base_dir; returns the local paths (NA for failures)
pb_download_files <- function(files, local_base_dir, skip_existing = TRUE) {
  paths <- rep(NA_character_, nrow(files))
  for (i in seq_len(nrow(files))) {
    dest <- file.path(local_base_dir, files$file_name[i])
    paths[i] <- dest
    if (skip_existing && file.exists(dest) &&
        file.size(dest) == files$size[i]) {
      next
    }
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    ok <- pb_try(
      pb_fetch_file(files$file_id[i], files$file_name[i], files$size[i], dest))
    if (is.null(ok)) {
      paths[i] <- NA_character_
      message("Failed to download ", files$file_name[i])
    }
  }
  paths
}
