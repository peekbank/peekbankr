# peekbankr reads data from the versioned peekbank dataset hosted on Redivis
# (https://redivis.com/datapages/datasets/peekbank). This file contains the
# machinery for resolving Peekbank releases to Redivis dataset versions and
# for fetching tables and running queries against them.
#
# Version resolution is intentionally not hardcoded: the "current" version is
# whatever the Redivis dataset's latest release is, and each Redivis version
# carries a one-row `release_info` table naming the Peekbank release it holds
# (e.g. "2026.1"), so the mapping travels with the data.

# session-level caches (release map, small whole-table fetches)
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

# CRAN policy requires graceful failure on unavailable internet resources:
# transient errors are retried with backoff, then produce a message and
# NULL -- never an error (the retry count is an option so tests can
# simulate an outage without waiting out the backoff)
pb_try <- function(expr, tries = getOption("peekbankr.request_tries", 3)) {
  expr <- substitute(expr)
  env <- parent.frame()
  for (i in seq_len(tries)) {
    result <- tryCatch(eval(expr, env), error = function(e) {
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

# muffle the redivis client's advisory warning about unqualified references
# (table names are resolved within the pinned dataset version)
quiet_redivis <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("No reference id was provided", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# reference to the peekbank Redivis dataset, optionally at a version tag
pb_dataset <- function(tag = NULL) {
  ds <- redivis::redivis$organization(peekbank_organization)
  if (is.null(tag)) {
    ds$dataset(peekbank_dataset_reference)
  } else {
    ds$dataset(peekbank_dataset_reference, version = tag)
  }
}

# read the release_info table at a given version tag -> release name string
release_name_at <- function(tag) {
  key <- paste0("release_name_", tag)
  if (is.null(.peekbankr_env[[key]])) {
    info <- pb_try(quiet_redivis(
      pb_dataset(tag)$table("release_info")$to_tibble()))
    if (is.null(info) || nrow(info) == 0) return(NULL)
    .peekbankr_env[[key]] <- info$release_name[[1]]
  }
  .peekbankr_env[[key]]
}

# map of Peekbank release names to Redivis version tags, discovered from the
# dataset itself (newest first), cached per session
release_map <- function() {
  if (!is.null(.peekbankr_env$release_map)) return(.peekbankr_env$release_map)
  if (!redivis_available()) return(NULL)
  props <- pb_try(quiet_redivis(pb_dataset()$get()$properties))
  if (is.null(props)) return(NULL)
  current_tag <- props$version$tag
  current_num <- as.integer(sub("^v1\\.", "", current_tag))
  tags <- paste0("v1.", seq(current_num, 0))
  names <- vapply(tags, function(tag) {
    nm <- release_name_at(tag)
    if (is.null(nm)) NA_character_ else nm
  }, character(1))
  map <- stats::setNames(tags[!is.na(names)], names[!is.na(names)])
  .peekbankr_env$release_map <- list(current_tag = current_tag, map = map)
  .peekbankr_env$release_map
}

# resolve a db_version argument ("current", "2025.1", or a raw tag "v1.2")
# to a release name + Redivis version tag
resolve_version <- function(db_version = "current") {
  rm_ <- release_map()
  if (is.null(rm_)) {
    stop("Could not reach the peekbank dataset on Redivis to resolve ",
         "versions.", call. = FALSE)
  }
  if (identical(db_version, "current")) {
    tag <- rm_$current_tag
    release <- names(rm_$map)[rm_$map == tag]
    message("Using current database version: '", release,
            "' (Redivis version ", tag, ").")
  } else if (db_version %in% names(rm_$map)) {
    tag <- unname(rm_$map[[db_version]])
    release <- db_version
    message("Using database version: '", release,
            "' (Redivis version ", tag, ").")
  } else if (db_version %in% rm_$map) {
    tag <- db_version
    release <- names(rm_$map)[rm_$map == tag]
    message("Using database version: '", release,
            "' (Redivis version ", tag, ").")
  } else {
    stop("Version '", db_version, "' not found. Available versions: ",
         "'current', ",
         paste(sprintf("'%s'", names(rm_$map)), collapse = ", "), ".",
         call. = FALSE)
  }
  list(release = release, tag = tag)
}

# fetch a whole table as a tibble, cached per session + version
pb_table <- function(connection, name) {
  tag <- connection$tag
  key <- paste(tag, name)
  if (is.null(.peekbankr_env[[key]])) {
    if (!redivis_available()) return(NULL)
    .peekbankr_env[[key]] <- pb_try(quiet_redivis(
      pb_dataset(tag)$table(name)$to_tibble()))
  }
  .peekbankr_env[[key]]
}

# run a SQL query against the dataset version (server-side filtering for the
# big timepoint tables); BigQuery Standard SQL dialect
pb_query <- function(connection, sql) {
  if (!redivis_available()) return(NULL)
  pb_try(quiet_redivis(pb_dataset(connection$tag)$query(sql)$to_tibble()))
}

# SQL IN (...) condition over integer ids; drops NAs (in MySQL,
# `IN (..., NULL)` never matched NULL -- dropping preserves that behavior)
sql_id_filter <- function(column, ids) {
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) return("FALSE")
  sprintf("%s IN (%s)", column,
          paste(format(ids, scientific = FALSE, trim = TRUE), collapse = ", "))
}

#' @export
print.peekbank_connection <- function(x, ...) {
  cat("<peekbank_connection> release ", x$release,
      " (Redivis ", peekbank_organization, "/", peekbank_dataset_reference,
      " version ", x$tag, ")\n", sep = "")
  invisible(x)
}

# ---- data files (raw_data / processed_data / READMEs) -----------------------
# Peekbank's files live in the companion datapages.peekbank_files dataset on
# Redivis: a single file-index table `files` whose file names are the
# OSF-era relative paths (<dataset>/raw_data/..., <dataset>/README.md).

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
  pb_try(quiet_redivis(pb_files_dataset()$query(sql)$to_tibble()))
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
    scratch <- file.path(tempdir(), paste0("peekbankr_dl_", i))
    dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
    ok <- pb_try({
      f <- redivis::redivis$file(files$file_id[i])
      f$get()
      f$download(path = scratch, overwrite = TRUE)
      TRUE
    })
    got <- list.files(scratch, full.names = TRUE)
    if (isTRUE(ok) && length(got) == 1) {
      file.rename(got[1], dest)
    } else {
      paths[i] <- NA_character_
      message("Failed to download ", files$file_name[i])
    }
    unlink(scratch, recursive = TRUE)
  }
  paths
}
