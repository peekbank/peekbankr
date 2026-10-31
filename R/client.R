#' Peekbank client
#'
#' A client pins one Peekbank release, so every table read through it comes
#' from the same version of the data. Create one with [peekbank()].
#'
#' Every table is reachable three ways:
#' * `pb$administrations` for the whole table, fetched once per session and reused after that
#' * `pb$subset$administrations()` to narrow it with `dplyr` predicates
#' * `pb$db$administrations` for a `dbplyr` handle that defers transfer until `dplyr::collect()`.
#'
#' @examples
#' \dontrun{
#' pb <- peekbank("2026.1")
#'
#' pb$administrations
#' pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")
#' pb$db$aoi_timepoints |> dplyr::count(aoi) |> dplyr::collect()
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect
#' @importFrom dbplyr sql_render
#' @export
PeekbankClient <- R6Class(
  "PeekbankClient",
  cloneable = FALSE,
  public = list(

    #' @description Pin a database version.
    #' @param version A Peekbank release name such as "2025.1", or "current"
    #'   for whichever was published most recently. Required: see
    #'   [peekbank()].
    initialize = function(version) {
      if (missing(version)) stop(version_required(), call. = FALSE)
      private$con <- new_connection(version)
      invisible(self)
    },

    #' @description Run a BigQuery Standard SQL query against this release
    #' @param sql A valid SQL query string
    query = function(sql) pb_query(private$con, sql),

    #' @description Download the stimulus images of this release's stimuli
    #' @param datasets Dataset names to download for; all datasets if empty
    #' @param dir Directory to save the images to
    #' @param skip_existing Skip an image that already exists locally with the
    #'   same size
    #' @return The stimuli that have an image, with `local_image_path` added
    #'   (`NA` where a download failed)
    images = function(datasets = c(), dir = "stimulus_data",
                      skip_existing = TRUE) {
      fetch_stimulus_images(private$con, datasets = datasets, dir = dir,
                            skip_existing = skip_existing)
    },

    #' @description Download the README of each of this release's datasets
    #' @param datasets Dataset names to download for; all datasets if empty
    #' @param dir Directory to save the READMEs to, as `<dataset_name>.md`
    #' @return The datasets table with the README's local path in
    #'   `local_readme_path` and its text in `readme` (`NA` where a download
    #'   failed)
    readmes = function(datasets = c(), dir = "dataset_readmes") {
      fetch_readmes(private$con, datasets = datasets, dir = dir)
    },

    #' @description Drop this version's (instance's) cached tables and query results, so the
    #'   next read fetches them again.
    clear_cache = function() {
      keys <- ls(.peekbankr_env)
      hits <- keys[startsWith(keys, paste0(private$con$redivis_tag, " "))]
      rm(list = hits, envir = .peekbankr_env)
      invisible(self)
    },

    #' @description Print the pinned release and what is reachable on it
    #' @param ... Ignored
    print = function(...) {
      cat("<peekbank> release ", private$con$release,
          " (Redivis version ", private$con$redivis_tag, ")\n", sep = "")
      methods <- setdiff(names(PeekbankClient$public_methods),
                         c("initialize", "print", "clone"))
      wrap <- function(label, x) {
        cat(strwrap(paste(x, collapse = "  "), initial = label,
                    prefix = strrep(" ", nchar(label)), width = 76), sep = "\n")
      }
      wrap("  tables:  ", peekbank_tables)
      wrap("  methods: ", paste0(sort(methods), "()"))
      cat("  subset:  $subset$<table>(<dplyr predicates>)\n")
      cat("  db:      $db$<table> for a dbplyr handle, read on collect()\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field release The Peekbank release this client is pinned to
    release = function(value) {
      private$assert_readonly(missing(value), "release")
      private$con$release
    },

    #' @field redivis_tag The Redivis version this client is pinned to
    redivis_tag = function(value) {
      private$assert_readonly(missing(value), "redivis_tag")
      private$con$redivis_tag
    },

    #' @field tables Names of the tables in this release
    tables = function(value) {
      private$assert_readonly(missing(value), "tables")
      fetch_table_names(private$con)
    },

    #' @field subset One function per table taking `dplyr` predicates, e.g.
    #'   `pb$subset$aoi_timepoints(age < 24)`. A predicate can name any
    #'   column of the table or of anything a row of it belongs to: an
    #'   administration's dataset and subject; a trial's trial type, with its
    #'   target and distractor stimuli (prefixed `target_` and `distractor_`)
    #'   and its AOI region set; a timepoint's administration and trial. The
    #'   result has the table's own columns either way. For the timepoint
    #'   tables the predicates run on the server to reduce transfer size,
    #'   except those naming `t_norm` on `aoi_timepoints`, which run on the
    #'   downloaded samples.
    subset = function(value) {
      private$assert_readonly(missing(value), "subset")
      if (is.null(private$subset_env)) {
        # The timepoint tables are large, so theirs filter on the server; the
        # rest are small enough that narrowing the cached tables is simpler
        # and faster than a round trip.
        e <- new.env(parent = emptyenv())
        for (nm in peekbank_tables) {
          local({
            name <- nm
            e[[name]] <- if (identical(name, "aoi_timepoints")) {
              function(...) {
                private$timepoints("aoi_timepoints_rle", rlang::enquos(...))
              }
            } else if (identical(name, "xy_timepoints")) {
              function(...) {
                private$timepoints("xy_timepoints", rlang::enquos(...))
              }
            } else {
              function(...) private$narrow(name, rlang::enquos(...))
            }
          })
        }
        class(e) <- "peekbank_subset_tables"
        private$subset_env <- e
      }
      private$subset_env
    },

    #' @field db `dbplyr` handles on this release's tables, one field each.
    #'   They carry the same columns as the fields above, but they are lazy queries,
    #'   so nothing is transferred until `dplyr::collect()`.
    db = function(value) {
      private$assert_readonly(missing(value), "db")
      if (is.null(private$db_env)) {
        e <- new.env(parent = emptyenv())
        for (nm in setdiff(fetch_table_names(private$con), "release_info")) {
          local({
            name <- nm
            makeActiveBinding(
              name,
              function() table_view(name, private$redivis_remote_table), e)
          })
        }
        class(e) <- "peekbank_db_tables"
        private$db_env <- e
      }
      private$db_env
    },

    #' @field datasets The datasets table
    datasets = function(value) {
      private$assert_readonly(missing(value), "datasets")
      fetch_table(private$con, "datasets")
    },

    #' @field subjects The subjects table. Subjects link longitudinal
    #'   administrations; `administrations` is usually the one you want.
    subjects = function(value) {
      private$assert_readonly(missing(value), "subjects")
      fetch_table(private$con, "subjects")
    },

    #' @field administrations The administrations table, with `dataset_name`
    #'   joined on
    administrations = function(value) {
      private$assert_readonly(missing(value), "administrations")
      fetch_table(private$con, "administrations")
    },

    #' @field trials The trials table, with `dataset_id` and `dataset_name`
    #'   joined on
    trials = function(value) {
      private$assert_readonly(missing(value), "trials")
      fetch_table(private$con, "trials")
    },

    #' @field trial_types The trial types table, with `dataset_name` joined on
    trial_types = function(value) {
      private$assert_readonly(missing(value), "trial_types")
      fetch_table(private$con, "trial_types")
    },

    #' @field stimuli The stimuli table, with `dataset_name` joined on
    stimuli = function(value) {
      private$assert_readonly(missing(value), "stimuli")
      fetch_table(private$con, "stimuli")
    },

    #' @field aoi_region_sets The AOI region sets table
    aoi_region_sets = function(value) {
      private$assert_readonly(missing(value), "aoi_region_sets")
      fetch_table(private$con, "aoi_region_sets")
    },

    #' @field aoi_timepoints Every AOI timepoint in the release: 36 million
    #'   rows, a little over half a gigabyte once expanded. It transfers
    #'   run-length encoded and is expanded locally.
    #'   `pb$subset$aoi_timepoints()` filters on the server instead.
    aoi_timepoints = function(value) {
      private$assert_readonly(missing(value), "aoi_timepoints")
      private$timepoints("aoi_timepoints_rle", list())
    },

    #' @field xy_timepoints Every XY timepoint in the release: 10 million rows
    xy_timepoints = function(value) {
      private$assert_readonly(missing(value), "xy_timepoints")
      private$timepoints("xy_timepoints", list())
    }
  ),

  private = list(
    con = NULL,
    dbi_con = NULL,
    db_env = NULL,
    subset_env = NULL,
    views = list(),

    assert_readonly = function(ok, name) {
      if (!ok) stop("`", name, "` is read-only.", call. = FALSE)
    },

    # one DBI connection per client, opened the first time it is needed
    dbi = function() {
      if (is.null(private$dbi_con)) {
        private$dbi_con <- dbConnect(redivis::RedivisDBI())
      }
      private$dbi_con
    },

    # fully qualified path to a table in this client's version
    path = function(name) {
      paste0(peekbank_organization, ".", peekbank_dataset_reference, ":",
             gsub(".", "_", private$con$redivis_tag, fixed = TRUE), ".", name)
    },

    redivis_remote_table = function(name) {
      quiet_redivis(dplyr::tbl(private$dbi(), private$path(name)))
    },

    # the predicate view for a timepoint table, kept because building it is
    # a schema lookup per table joined
    db_predicate_view = function(name) {
      if (is.null(private$views[[name]])) {
        private$views[[name]] <-
          predicate_view(name, private$redivis_remote_table)
      }
      private$views[[name]]
    },

    # Rows of one timepoint table, for those matching `dots`.
    # Predicates reach everything either the foreign keys lead to.
    timepoints = function(name, dots) {
      encoded <- identical(name, "aoi_timepoints_rle")
      public_name <- if (encoded) "aoi_timepoints" else name
      vars <- lapply(dots, function(q) all.vars(rlang::quo_get_expr(q)))

      # The encoded table is transferred as runs of samples, which the user
      # never sees. `length` only exists on the runs, and `t_norm` there is
      # where a run starts, so predicates naming `t_norm` run on the decoded
      # samples instead, with any other columns they name carried along.
      if (encoded && "length" %in% unlist(vars)) {
        stop("No column `length` to filter aoi_timepoints on. It only ",
             "exists on the run-length\n  encoded table, ",
             "pb$db$aoi_timepoints_rle.", call. = FALSE)
      }
      on_samples <- vapply(vars, function(v) encoded && "t_norm" %in% v,
                           logical(1))
      sample_dots <- dots[on_samples]
      run_dots <- dots[!on_samples]

      query <- self$db[[name]]
      result_columns <- colnames(query)
      carry <- character()
      if (length(dots) > 0) {
        view <- private$db_predicate_view(name)
        private$reject_unknown_columns(
          dots, setdiff(colnames(view), "length"), public_name)
        carry <- setdiff(intersect(unlist(vars[on_samples]), colnames(view)),
                         result_columns)
        if (length(run_dots) > 0 || length(carry) > 0) {
          query <- view %>%
            dplyr::filter(!!!run_dots) %>%
            dplyr::select(dplyr::all_of(c(result_columns, carry)))
        }
      }
      order <- if (identical(name, "xy_timepoints")) "xy_timepoint_id" else
        c("administration_id", "trial_id", "t_norm")
      sql <- as.character(
        sql_render(dplyr::arrange(query, !!!rlang::syms(order))))
      rows <- if (length(run_dots) == 0) pb_query(private$con, sql) else
        tryCatch(pb_query(private$con, sql), error = function(e) {
          stop(conditionMessage(e), "\n  Predicates on ", public_name,
               " are translated to SQL. Use !! for values computed by a\n  ",
               "function in R. The functions that translate are listed at\n  ",
               "https://dplyr.tidyverse.org/reference/filter.html",
               "#useful-filter-functions\n  and\n  ",
               "https://docs.redivis.com/api/client-libraries/redivis-r/",
               "reference/redivis/redivisusdconnect_dbi",
               "#supported-dplyr-functions", call. = FALSE)
        })

      if (is.null(rows)) return(NULL)
      if (!encoded) return(rows)
      # keep the expanded rows too
      key <- paste(private$con$redivis_tag, "decoded", sql)
      if (is.null(.peekbankr_env[[key]])) {
        .peekbankr_env[[key]] <- decode_rle_timepoints(rows, carry)
      }
      if (length(sample_dots) == 0) return(.peekbankr_env[[key]])
      .peekbankr_env[[key]] %>%
        dplyr::filter(!!!sample_dots) %>%
        dplyr::select(-dplyr::all_of(carry))
    },

    # A table narrowed by predicates that may name anything a row of it
    # belongs to. These tables are small, so the widening happens on the
    # cached copies to save time and roundtrips (and have access to more predicte expressions)
    narrow = function(name, dots) {
      cols <- names(self[[name]])
      view <- predicate_view(name, function(n) pb_table(private$con, n))
      private$reject_unknown_columns(dots, colnames(view), name)
      view %>%
        dplyr::filter(!!!dots) %>%
        dplyr::select(dplyr::all_of(cols))
    },

    # dplyr says "object `x` not found", which does not say what could have
    # been named instead.
    reject_unknown_columns = function(dots, cols, table_name) {
      unknown <- unlist(lapply(dots, function(q) {
        vars <- setdiff(all.vars(rlang::quo_get_expr(q)), c(".data", ".env"))
        is_local <- vapply(vars, exists, logical(1),
                           envir = rlang::quo_get_env(q))
        vars[!vars %in% cols & !is_local]
      }))
      if (length(unknown) == 0) return(invisible(NULL))

      filterable <- strwrap(paste("Can filter on:",
                                  paste(sort(cols), collapse = ", ")),
                            width = 74, exdent = 2)
      stop("No column `", unknown[1], "` to filter ", table_name, " on.",
           "\n  ", paste(filterable, collapse = "\n  "), call. = FALSE)
    }
  )
)

peekbank_tables <- c("datasets", "subjects", "administrations", "trials",
                     "trial_types", "stimuli", "aoi_region_sets",
                     "aoi_timepoints", "xy_timepoints")

# `pb$query` without the parentheses is an easy slip, and a bare closure
# prints its source and environment address. Tag the methods so they say what
# to do instead.
for (nm in setdiff(names(PeekbankClient$public_methods),
                   c("initialize", "print", "clone"))) {
  class(PeekbankClient$public_methods[[nm]]) <- c("peekbank_method", "function")
}


# Reprogramming object access to fail loudly.
# A member of `x`, or an error naming what is there. R hands back NULL for a
# name an environment does not have, which surfaces as a puzzle several steps
# later in a pipe rather than at the typo.
member <- function(x, name, what) {
  if (name %in% ls(x, all.names = TRUE)) {
    return(get(name, envir = x, inherits = FALSE))
  }
  # initialize is R6's constructor; peekbank() is the way in
  stop("No ", what, " called `", name, "`.\n  Available: ",
       paste(setdiff(sort(ls(x)), "initialize"), collapse = ", "),
       call. = FALSE)
}

#' @export
`$.PeekbankClient` <- function(x, name) member(x, name, "field or method")

#' @export
`[[.PeekbankClient` <- function(x, name) member(x, name, "field or method")

#' @export
`$.peekbank_db_tables` <- function(x, name) member(x, name, "table")

#' @export
`[[.peekbank_db_tables` <- function(x, name) member(x, name, "table")

#' @export
`$.peekbank_subset_tables` <- function(x, name) member(x, name, "table")

#' @export
`[[.peekbank_subset_tables` <- function(x, name) member(x, name, "table")

#' @export
print.peekbank_db_tables <- function(x, ...) {
  cat("<peekbank database tables>\n")
  cat(strwrap(paste(sort(ls(x)), collapse = "  "), prefix = "  ", width = 76),
      sep = "\n")
  invisible(x)
}

#' @export
print.peekbank_subset_tables <- function(x, ...) {
  cat("<peekbank subset> call one with dplyr predicates,",
      "e.g. $aoi_timepoints(age < 24)\n")
  cat(strwrap(paste0(sort(ls(x)), "()", collapse = "  "), prefix = "  ",
              width = 76), sep = "\n")
  invisible(x)
}

#' @export
print.peekbank_method <- function(x, ...) {
  cat("<peekbank method> call it with parentheses, e.g. pb$query()\n")
  invisible(x)
}

#' Connect to Peekbank
#'
#' Returns a client pinned to one Peekbank release. Every table read through it
#' comes from that version, so an analysis cannot mix releases.
#'
#' @param version A Peekbank release name such as "2025.1". Pass "current" to
#'   take whichever version was published most recently
#'
#' @return A [PeekbankClient]
#' @export
#'
#' @examples
#' \dontrun{
#' pb <- peekbank("2026.1")
#' pb
#'
#' # whole tables
#' administrations <- pb$administrations
#'
#' # narrowed, on the server for the timepoint tables
#' aoi <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")
#'
#' # computed on the server, nothing transferred until collect()
#' pb$db$aoi_timepoints |> dplyr::count(aoi) |> dplyr::collect()
#' }
peekbank <- function(version) {
  if (missing(version)) stop(version_required(), call. = FALSE)
  PeekbankClient$new(version = version)
}

#' Peekbank releases
#'
#' The releases [peekbank()] can be pinned to, newest Redivis version first.
#'
#' The `current` release is the one `peekbank("current")` resolves to: the
#' most recently published Redivis version.
#'
#' @return A tibble of one row per release, with the Peekbank `release`
#'   name, the `redivis_tag` of the version holding it, and whether it is
#'   `current`
#' @export
#'
#' @examples
#' \dontrun{
#' peekbank_releases()
#' }
peekbank_releases <- function() {
  idx <- published_redivis_tags()
  if (is.null(idx)) return(NULL)
  names <- vapply(idx$redivis_tags, function(redivis_tag) {
    nm <- release_name_at(redivis_tag)
    if (is.null(nm)) NA_character_ else nm
  }, character(1))
  found <- !is.na(names)
  dplyr::tibble(
    release = unname(names[found]),
    redivis_tag = idx$redivis_tags[found],
    current = idx$redivis_tags[found] == idx$current_redivis_tag
  )
}

# the error for a missing version, listing what could have been named
version_required <- function() {
  releases <- tryCatch(suppressMessages(peekbank_releases()),
                       error = function(e) NULL)
  available <- if (is.null(releases)) "" else paste0(
    "  Available: ",
    paste(sprintf("\"%s\"", releases$release), collapse = ", "), "\n")
  paste0("`version` is required, so a script records which release it read.\n",
         available,
         "  Or \"current\" for whichever was published most recently.")
}
