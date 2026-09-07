#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
#' @importFrom rlang ":="
NULL

options(warn = -1)

validate_dataset_args <- function(dataset_id, dataset_name) {
  if (!is.null(dataset_id) && !is.numeric(dataset_id)) {
    stop("dataset_id must be numeric. Did you mean to use dataset_name = \"", dataset_id, "\"?")
  }
  if (!is.null(dataset_name) && !is.character(dataset_name)) {
    stop("dataset_name must be a character string.")
  }
}

pkg_globals <- new.env()
pkg_globals$SAMPLE_RATE <- 40 # Hz

#' Get information on database connection options
#'
#' As of peekbankr 0.4, data are retrieved from the versioned peekbank
#' dataset on Redivis (\url{https://redivis.com/datapages/datasets/peekbank})
#' rather than a MySQL server. Version information is discovered from the
#' Redivis dataset itself: each Redivis version carries a `release_info`
#' table naming the Peekbank release it holds.
#'
#' @return List of database info: `current` (the Peekbank release that is the
#'   latest Redivis version) and `versions` (a named vector mapping Peekbank
#'   release names to Redivis dataset version tags)
#' @export
#'
#' @examples
#' \dontrun{
#' get_db_info()
#' }
get_db_info <- function() {
  rm_ <- release_map()
  if (is.null(rm_)) return(NULL)
  list(
    current = names(rm_$map)[rm_$map == rm_$current_tag],
    versions = rm_$map
  )
}

#' Connect to Peekbank
#'
#' As of peekbankr 0.4, data are retrieved from the versioned peekbank
#' dataset on Redivis rather than a MySQL server. `connect_to_peekbank()`
#' now returns a lightweight handle that pins a database version (Redivis
#' dataset version); it no longer opens a DBI connection. Existing code that
#' passes the result via the `connection` argument of the `get_` functions
#' continues to work.
#'
#' @param db_version String of the name of database version to use: "current"
#'   (the default; resolves to the latest Redivis version), a Peekbank
#'   release name (e.g. "2025.1"), or a Redivis version tag (e.g. "v1.2")
#' @param db_args Deprecated, ignored (retained for backwards compatibility)
#' @param compress Deprecated, ignored
#' @param host Deprecated, ignored
#' @param port Deprecated, ignored
#' @param ssl Deprecated, ignored
#'
#' @return A `peekbank_connection` handle pinning the resolved version
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_peekbank(db_version = "current")
#' }
connect_to_peekbank <- function(db_version = "current", db_args = NULL,
                                compress = TRUE, host = NULL, port = NULL,
                                ssl = "auto") {
  supplied <- c("db_args", "compress", "host", "port", "ssl")[
    c(!missing(db_args), !missing(compress), !missing(host), !missing(port),
      !missing(ssl))]
  if (length(supplied) > 0) {
    warning("peekbankr now reads from the peekbank dataset on Redivis. ",
            "These arguments are deprecated and ignored: ",
            paste(supplied, collapse = ", "), ".", call. = FALSE)
  }
  ver <- resolve_version(db_version)
  structure(
    list(release = ver$release, tag = ver$tag),
    class = "peekbank_connection"
  )
}

resolve_connection <- function(connection) {
  if (is.null(connection)) {
    warning("No connection provided. Defaulting to connect_to_peekbank(db_version = 'current'). ",
            "This can result in mismatched database versions if you are using a different version elsewhere. ",
            "This implicit behavior is deprecated and will be removed in a future version. ",
            "Please create a connection explicitly with connect_to_peekbank() and pass it via the connection argument.",
            call. = FALSE)
    connect_to_peekbank()
  } else if (inherits(connection, "peekbank_connection")) {
    connection
  } else {
    stop("`connection` must be a peekbank_connection from ",
         "connect_to_peekbank(). As of peekbankr 0.4, DBI/MySQL connections ",
         "are no longer supported.", call. = FALSE)
  }
}

#' List of peekbank tables
#'
#' @param connection A connection to the peekbank database
#'
#' @return A vector of the names of tables in peekbank
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_peekbank()
#' list_peekbank_tables(con)
#' }
list_peekbank_tables <- function(connection) {
  con <- resolve_connection(connection)
  tables <- pb_try(quiet_redivis(
    vapply(pb_dataset(con$tag)$list_tables(), function(t) {
      if (!is.null(t$name)) t$name else t$properties$name
    }, character(1))))
  if (is.null(tables)) return(NULL)
  sort(unname(tables))
}

#' Get datasets
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of Datasets data.
#' @export
#'
#' @examples
#' \dontrun{
#' get_datasets()
#' }
get_datasets <- function(connection = NULL) {
  con <- resolve_connection(connection)
  pb_table(con, "datasets")
}

count_datasets <- function(datasets) {
  datasets %>%
    dplyr::collect() %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)
}

#' Get administrations
#'
#' @param age A numeric vector of a single age or a min age and max age
#'   (inclusive), in months
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of Administrations data, filtered down by supplied
#'   arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_administrations()
#' get_administrations(age = c())
#' get_administrations(dataset_name = "pomper_saffran_2016")
#' }
get_administrations <- function(age = NULL, dataset_id = NULL,
                                dataset_name = NULL, connection = NULL) {
  validate_dataset_args(dataset_id, dataset_name)
  con <- resolve_connection(connection)
  input_age <- age
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  administrations <- pb_table(con, "administrations")
  datasets <- pb_table(con, "datasets")

  if (!is.null(dataset_id)) {
    datasets %<>%
      dplyr::filter(.data$dataset_id %in% input_dataset_id)
  }
  if (!is.null(dataset_name)) {
    datasets %<>%
      dplyr::filter(.data$dataset_name %in% input_dataset_name)
  }

  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  if (!is.null(input_age)) {
    if (length(input_age) == 1) {
      administrations %<>% dplyr::filter(.data$age == input_age)
    } else if (length(input_age) == 2) {
      min_age <- input_age[1]
      max_age <- input_age[2]
      administrations %<>% dplyr::filter(.data$age >= min_age &
                                           .data$age <= max_age)
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  datasets %<>% dplyr::select(.data$dataset_id, .data$dataset_name)
  administrations %<>% dplyr::inner_join(datasets, by = "dataset_id")

  return(administrations)
}


#' Get subjects
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of Subjects data. Note that Subjects is a table used to
#'   link longitudinal Administrations, which is the primary table you probably
#'   want.
#' @export
#'
#' @examples
#' \dontrun{
#' get_subjects()
#' }
get_subjects <- function(connection = NULL) {
  con <- resolve_connection(connection)
  pb_table(con, "subjects")
}

#' Get trials
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of Trials data, filtered down by supplied arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_trials()
#' get_trials(dataset_name = "pomper_saffran_2016")
#' }
get_trials <- function(dataset_id = NULL, dataset_name = NULL,
                       connection = NULL) {
  validate_dataset_args(dataset_id, dataset_name)
  con <- resolve_connection(connection)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  trials <- pb_table(con, "trials")
  trial_types <- pb_table(con, "trial_types")

  datasets <- pb_table(con, "datasets")
  if (!is.null(dataset_id)) {
    datasets %<>%
      dplyr::filter(.data$dataset_id %in% input_dataset_id)
  }
  if (!is.null(dataset_name)) {
    datasets %<>%
      dplyr::filter(.data$dataset_name %in% input_dataset_name)
  }
  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  trial_types %<>% dplyr::select(.data$trial_type_id, .data$dataset_id)
  datasets %<>% dplyr::select(.data$dataset_id, .data$dataset_name)
  trials %<>%
    dplyr::left_join(trial_types, by = "trial_type_id") %>%
    dplyr::inner_join(datasets, by = "dataset_id")

  return(trials)
}

#' Get trial types
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of Trial Types data, filtered down by supplied arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_trial_types()
#' get_trial_types(dataset_name = "pomper_saffran_2016")
#' }
get_trial_types <- function(dataset_id = NULL, dataset_name = NULL,
                            connection = NULL) {
  validate_dataset_args(dataset_id, dataset_name)
  con <- resolve_connection(connection)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  trial_types <- pb_table(con, "trial_types")

  datasets <- pb_table(con, "datasets")
  if (!is.null(dataset_id)) {
    datasets %<>%
      dplyr::filter(.data$dataset_id %in% input_dataset_id)
  }
  if (!is.null(dataset_name)) {
    datasets %<>%
      dplyr::filter(.data$dataset_name %in% input_dataset_name)
  }
  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  datasets %<>% dplyr::select(.data$dataset_id, .data$dataset_name)
  trial_types %<>% dplyr::inner_join(datasets, by = "dataset_id")

  return(trial_types)
}


#' Get stimuli
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of Stimuli data, filtered down by supplied arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_stimuli()
#' get_stimuli(dataset_name = "pomper_saffran_2016")
#' }
get_stimuli <- function(dataset_id = NULL, dataset_name = NULL,
                        connection = NULL) {
  validate_dataset_args(dataset_id, dataset_name)
  con <- resolve_connection(connection)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  stimuli <- pb_table(con, "stimuli")

  datasets <- pb_table(con, "datasets")
  if (!is.null(dataset_id)) {
    datasets %<>%
      dplyr::filter(.data$dataset_id %in% input_dataset_id)
  }
  if (!is.null(dataset_name)) {
    datasets %<>%
      dplyr::filter(.data$dataset_name %in% input_dataset_name)
  }
  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  datasets %<>% dplyr::select(.data$dataset_id, .data$dataset_name)
  stimuli %<>% dplyr::inner_join(datasets, by = "dataset_id")

  return(stimuli)
}

#' Get AOI region sets
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A tibble of AOI Region Sets data, filtered down by supplied
#'   arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_aoi_region_sets()
#' }
get_aoi_region_sets <- function(connection = NULL) {
  con <- resolve_connection(connection)
  pb_table(con, "aoi_region_sets")
}

#' Get AOI timepoints
#'
#' @inheritParams get_trials
#' @inheritParams get_administrations
#' @param rle Logical indicating whether to use RLE data representation or not
#'
#' @return A tibble of AOI Timepoints data, filtered down by supplied
#'   arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_aoi_timepoints(dataset_name = "pomper_saffran_2016")
#' }
get_aoi_timepoints <- function(dataset_id = NULL, dataset_name = NULL,
                               age = NULL, rle = TRUE, connection = NULL) {
  con <- resolve_connection(connection)

  administrations <- get_administrations(
    age = age, dataset_id = dataset_id, dataset_name = dataset_name,
    connection = con
  )

  # if you are using the (default) RLE encoding, then get the RLE version
  # otherwise get the normal one; filtering happens server-side on Redivis.
  # ORDER BY matters: the RLE decode below assumes runs arrive in time order
  # within each administration x trial
  if (rle) {
    sql <- sprintf(
      "SELECT * FROM aoi_timepoints_rle WHERE %s ORDER BY administration_id, trial_id, t_norm",
      sql_id_filter("administration_id", administrations$administration_id))
  } else {
    sql <- sprintf(
      "SELECT * FROM aoi_timepoints WHERE %s ORDER BY aoi_timepoint_id",
      sql_id_filter("administration_id", administrations$administration_id))
  }
  aoi_timepoints <- pb_query(con, sql)
  if (is.null(aoi_timepoints)) return(NULL)

  # undo the RLE transform locally
  if (rle) {
    aoi_timepoints <- decode_rle_timepoints(aoi_timepoints)
  }

  return(aoi_timepoints)
}

# expand a run-length-encoded AOI timepoints table (one row per run of a
# constant AOI within an administration x trial, with t_norm the run start
# and `length` the run length in samples) back to one row per 25 ms sample.
# Rows must be ordered by t_norm within each administration x trial.
decode_rle_timepoints <- function(aoi_timepoints) {
  timestep <- 1000 / pkg_globals$SAMPLE_RATE

  aoi_timepoints %>%
    tidyr::nest(trial_data = -c(.data$administration_id, .data$trial_id)) %>%
    dplyr::mutate(
      rle_vector = purrr::map(.data$trial_data, function(td) {
        `class<-`(list(lengths = as.integer(td$length), values = td$aoi), "rle")
      }),
      aoi = purrr::map(.data$rle_vector, inverse.rle),
      t_norm = purrr::map(.data$trial_data, function(td) {
        as.integer(seq(
          td$t_norm[1], td$t_norm[1] + (sum(td$length) - 1) * timestep,
          timestep
        ))
      })
    ) %>%
    dplyr::select(-.data$trial_data, -.data$rle_vector) %>%
    tidyr::unnest(cols = c(.data$aoi, .data$t_norm))
}

#' Get XY timepoints
#'
#' @inheritParams get_trials
#' @inheritParams get_administrations
#'
#' @return A tibble of XY timepoints data, filtered down by supplied
#'   arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_xy_timepoints(dataset_name = "reflook_v4")
#' }
get_xy_timepoints <- function(dataset_id = NULL, dataset_name = NULL,
                              age = NULL, connection = NULL) {
  con <- resolve_connection(connection)

  administrations <- get_administrations(
    dataset_id = dataset_id,
    dataset_name = dataset_name,
    age = age, connection = con
  )

  sql <- sprintf(
    "SELECT * FROM xy_timepoints WHERE %s ORDER BY xy_timepoint_id",
    sql_id_filter("administration_id", administrations$administration_id))
  xy_timepoints <- pb_query(con, sql)

  return(xy_timepoints)
}


#' Unpack the json sting in the *_aux_data column and turns
#' it into a nested R list
#'
#'
#' @param df a dataframe in the peekbank format that has an aux data column
#'
#' @return the input dataframe, with the *_aux_data column unpacked
#'
#' @export
#'
#' @examples
#' \dontrun{
#' subjects_table <- unpack_aux_data(df = subjects_table)
#' }
unpack_aux_data <- function(df) {
  all_names <- colnames(df)
  aux_name <- all_names[stringr::str_which(all_names, ".*_aux_data$")]
  if (length(aux_name) == 0) {
    return(df)
  }
  aux_list <- df |>
    dplyr::ungroup() |>
    dplyr::pull(dplyr::all_of(aux_name)) |>
    lapply(\(aux) {
      if (is.na(aux) | is.null(aux)) {
        return(aux)
      }
      jsonlite::fromJSON(aux)
    })
  if (all(is.na(aux_list))) {
    return(df)
  }

  col_names <- purrr::flatten(aux_list) |>
    names() |>
    unique()
  col_names <- col_names[!is.na(col_names) & col_names != ""]

  aux_cols <- lapply(col_names, \(col_name) {
    sapply(aux_list, \(aux) {
      # cursed way to make sure that there are no "NULL" strings left over
      # from weird jsonlite::fromJSON behavior,
      # check https://github.com/jeroen/jsonlite/issues/70 to see if there has been a fix by now
      if (length(aux) == 1 &&
          (is.na(aux) ||
           is.null(aux[col_name]) ||
           aux[col_name] == "NULL"
          ) || (
            all(is.na(aux)) ||
            all(is.null(aux[col_name])) ||
            all(aux[col_name] == "NULL")
          )
      ) {
        return(NA)
      }
      aux[col_name]
    })
  }) |>
    `names<-`(value = col_names) |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(aux) {
      if (any(sapply(aux, \(aux_val) {
        typeof(aux_val) == "list"
      }))) {
        aux <- lapply(aux, \(aux_val) {
          if (all(is.na(aux_val))) {
            return(NA)
          }
          dplyr::bind_rows(aux_val)
        })
      }
      if (all(sapply(aux, is.atomic))) {
        aux <- purrr::list_simplify(aux, strict = FALSE) # May need a better fix for NAs
      }
      aux
    }))
  df |>
    cbind(aux_cols) |>
    dplyr::select(-dplyr::all_of(aux_name)) |>
    tidyr::nest("{aux_name}" := dplyr::all_of(colnames(aux_cols)))
}

#' Run a SQL query against peekbank
#'
#' Queries are BigQuery Standard SQL, run against the peekbank dataset on
#' Redivis. Note that string comparison is case-sensitive.
#'
#' @param sql_query_string A valid SQL query string
#' @param connection A connection to the peekbank database
#'
#' @return A tibble of the query results
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_peekbank()
#' get_sql_query("SELECT * FROM datasets", connection = con)
#' }
get_sql_query <- function(sql_query_string, connection = NULL) {
  con <- resolve_connection(connection)
  pb_query(con, sql_query_string)
}


#' Download stimulus images for the Peekbank repository
#'
#' This function downloads stimulus images for selected Peekbank datasets from
#' the peekbank_files dataset on Redivis. It retrieves stimulus metadata from a
#' Peekbank database connection, constructs the full stimulus file paths, and
#' downloads them to a local directory.
#'
#' @param con A database connection object created by connect_to_peekbank()
#' @param local_base_dir Local directory path where stimulus images will be saved (default: "stimulus_data")
#' @param datasets Character vector of dataset names to download stimuli for.
#'                 If empty (default), downloads stimuli for all datasets.
#' @param skip_existing skip downloading a file if a file with that name already exists in that path locally
#'
#' @return Returns the stimulus df with an additional column for the paths of the downloaded stimuli
#'
#'
#' @examples
#' \dontrun{
#' con <- connect_to_peekbank("2025.1")
#'
#' # Download stimuli for all datasets
#' download_stimuli(con, local_base_dir = "stimulus_data")
#'
#' # Download stimuli for specific datasets
#' download_stimuli(con, local_base_dir = "stimulus_data",
#'                  datasets = c("reflook_v4", "reflook_socword"))
#' }
#'
#' @export
download_stimuli <- function(con, local_base_dir = "stimulus_data", datasets = c(),
                             skip_existing = TRUE) {
  stimuli_df <- get_stimuli(connection = con) %>%
    dplyr::collect() %>%
    dplyr::filter(!is.na(stimulus_image_path))

  if (length(datasets > 0)) {
    stimuli_df <- stimuli_df %>% dplyr::filter(dataset_name %in% datasets)
  }

  wanted <- stimuli_df %>%
    dplyr::mutate(full_stimulus_path = paste0(dataset_name, "/raw_data/",
                                              stimulus_image_path))

  # files live in the peekbank_files dataset on Redivis; one query per
  # dataset keeps the file listing small
  listings <- lapply(unique(wanted$dataset_name), function(ds) {
    pb_files_query(paste0(ds, "/raw_data/"))
  })
  index <- dplyr::bind_rows(listings)
  files <- dplyr::inner_join(
    wanted %>% dplyr::distinct(full_stimulus_path),
    index, by = c("full_stimulus_path" = "file_name")) %>%
    dplyr::rename(file_name = full_stimulus_path)

  missing <- setdiff(wanted$full_stimulus_path, files$file_name)
  if (length(missing) > 0) {
    message(length(missing), " stimulus files were not found in the ",
            "peekbank_files dataset (e.g. ", missing[1], ")")
  }

  local <- pb_download_files(files, local_base_dir,
                             skip_existing = skip_existing)
  path_map <- stats::setNames(local, files$file_name)
  message("Downloaded ", sum(!is.na(local)), " stimulus files from Redivis")

  return(wanted %>%
    dplyr::mutate(local_stimulus_path =
                    unname(path_map[full_stimulus_path])) %>%
    dplyr::select(-full_stimulus_path))
}


#' Download dataset README files
#'
#' Downloads README files for Peekbank datasets from the peekbank_files
#' dataset on Redivis. Note that READMEs reflect the latest released version
#' of the files dataset.
#'
#' @param datasets Character vector of dataset names. If empty (default),
#'   downloads READMEs for all datasets.
#' @param local_base_dir Directory to save README files to (default: "dataset_readmes")
#'
#' @return No return value, called for side effects. README files are saved to
#'   the specified directory and the path is printed via message.
#' @export
#'
#' @examples
#' \dontrun{
#' get_readmes()
#' get_readmes(datasets = c("pomper_saffran_2016"))
#' }
get_readmes <- function(datasets = c(), local_base_dir = "dataset_readmes") {
  # READMEs live at <dataset>/README.md in the peekbank_files dataset
  index <- pb_files_query("")
  if (is.null(index)) return(invisible(NULL))
  readmes <- index[grepl("^[^/]+/README\\.md$", index$file_name), ]
  if (length(datasets) > 0) {
    readmes <- readmes[sub("/README\\.md$", "", readmes$file_name) %in%
                         datasets, ]
  }

  if (!dir.exists(local_base_dir)) dir.create(local_base_dir, recursive = TRUE)

  staging_dir <- file.path(tempdir(), "peekbank_readmes_staging")
  local <- pb_download_files(readmes, staging_dir, skip_existing = FALSE)
  for (i in seq_len(nrow(readmes))) {
    if (is.na(local[i])) next
    ds_name <- sub("/README\\.md$", "", readmes$file_name[i])
    file.copy(local[i], file.path(local_base_dir, paste0(ds_name, ".md")),
              overwrite = TRUE)
  }
  unlink(staging_dir, recursive = TRUE)

  message("READMEs saved to: ", local_base_dir)
}
