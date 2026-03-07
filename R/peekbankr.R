#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
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

translate_version <- function(db_version, db_args, db_info) {
  # using the peekbankr hosted server

  if (db_args$host == db_info$host && db_args$port == db_info$port) {
    # current version
    if (db_version == "current") {
      db_to_use <- db_info[["current"]]
      message("Using current database version: '", db_to_use, "'.")
      return(db_to_use)

      # supported version
    } else if (db_version %in% db_info[["supported"]]) {
      db_to_use <- db_version
      message("Using supported database version: '", db_to_use, "'.")
      return(db_to_use)

      # historical version
    } else if (db_version %in% db_info[["historical"]]) {
      stop(
        "Version '", db_version, "' is no longer hosted by ",
        "peekbank.stanford.edu; either specify a more recent version or ",
        "install MySQL Server locally and update db_args."
      )

      # version not recognized
    } else {
      stop(
        "Version '", db_version, "' not supported. Our hosted instance currently offers: 'current', ",
        paste(sprintf("'%s'", db_info$supported), collapse = ", "), "."
      )
    }

    # using a different server than the peekbankr hosted one
  } else {
    message(
      "Not using default hosted Peekbank instance; no checks will be applied to ",
      "version specification."
    )
    return(db_version)
  }
}

#' Get information on database connection options
#'
#' @return List of database info: host name, current version, supported
#'   versions, historical versions, username, password
#' @export
#'
#' @examples
#' \dontrun{
#' get_db_info()
#' }
get_db_info <- function() {
  jsonlite::fromJSON("https://peekbank.github.io/peekbank-website/peekbank.json")
}

#' Connect to Peekbank
#'
#' @param db_version String of the name of database version to use
#' @param db_args List with host, user, and password defined
#' @param compress Flag to use compression protocol (defaults to TRUE)
#' @param host Hostname of the Peekbank server to connect to (defaults hosted PB)
#' @param port Port of the Peekbank DB to connect to (defaults to 3306)
#'
#' @return con A DBIConnection object for the peekbank database
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_peekbank(db_version = "current", db_args = NULL)
#' DBI::dbDisconnect(con)
#' }
connect_to_peekbank <- function(db_version = "current", db_args = NULL,
                                compress = TRUE, host = NULL, port = NULL) {
  db_info <- get_db_info()
  db_info$port <- if (!is.null(db_info$port)) db_info$port else 3307

  flags <- if (compress) RMariaDB::CLIENT_COMPRESS else 0

  if (is.null(db_args)) db_args <- db_info

  db_args$host <- if (!is.null(host)) host else db_args$host
  db_args$port <- if (!is.null(port)) port else db_info$port

  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = db_args$host,
    port = db_args$port,
    dbname = translate_version(db_version, db_args, db_info),
    user = db_args$user,
    password = db_args$password,
    client.flag = flags
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
  } else {
    connection
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
  DBI::dbListTables(connection)
}

#' Get datasets
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Datasets data. If `connection` is supplied, the result
#'   remains a remote query, otherwise it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_datasets()
#' }
get_datasets <- function(connection = NULL) {
  con <- resolve_connection(connection)

  datasets <- dplyr::tbl(con, "datasets")

  if (is.null(connection)) {
    datasets %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(datasets)
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
#' @return A `tbl` of Administrations data, filtered down by supplied arguments.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
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
  con <- resolve_connection(connection)
  input_age <- age
  validate_dataset_args(dataset_id, dataset_name)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  administrations <- dplyr::tbl(con, "administrations")
  datasets <- dplyr::tbl(con, "datasets")

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

  if (is.null(connection)) {
    administrations %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(administrations)
}


#' Get subjects
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Subjects data. Note that Subjects is a table used to link
#'   longitudinal Administrations, which is the primary table you probably want.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_subjects()
#' }
get_subjects <- function(connection = NULL) {
  con <- resolve_connection(connection)

  subjects <- dplyr::tbl(con, "subjects")

  if (is.null(connection)) {
    subjects %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(subjects)
}

#' Get trials
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Trials data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_trials()
#' get_trials(dataset_name = "pomper_saffran_2016")
#' }
get_trials <- function(dataset_id = NULL, dataset_name = NULL,
                       connection = NULL) {
  con <- resolve_connection(connection)
  validate_dataset_args(dataset_id, dataset_name)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  trials <- dplyr::tbl(con, "trials")
  trial_types <- dplyr::tbl(con, "trial_types")

  datasets <- dplyr::tbl(con, "datasets")
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

  if (is.null(connection)) {
    trials %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(trials)
}

#' Get trial types
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Trial Types data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_trial_types()
#' get_trial_types(dataset_name = "pomper_saffran_2016")
#' }
get_trial_types <- function(dataset_id = NULL, dataset_name = NULL,
                            connection = NULL) {
  con <- resolve_connection(connection)
  validate_dataset_args(dataset_id, dataset_name)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  trial_types <- dplyr::tbl(con, "trial_types")

  datasets <- dplyr::tbl(con, "datasets")
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

  if (is.null(connection)) {
    trial_types %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(trial_types)
}


#' Get stimuli
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param dataset_name A character vector of one or more dataset names
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Stimuli data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_stimuli()
#' get_stimuli(dataset_name = "pomper_saffran_2016")
#' }
get_stimuli <- function(dataset_id = NULL, dataset_name = NULL,
                        connection = NULL) {
  con <- resolve_connection(connection)
  validate_dataset_args(dataset_id, dataset_name)
  input_dataset_id <- dataset_id
  input_dataset_name <- dataset_name

  stimuli <- dplyr::tbl(con, "stimuli")

  datasets <- dplyr::tbl(con, "datasets")
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

  if (is.null(connection)) {
    stimuli %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(stimuli)
}

#' Get AOI region sets
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of AOI Region Sets data, filtered down by supplied arguments.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_aoi_region_sets()
#' }
get_aoi_region_sets <- function(connection = NULL) {
  con <- resolve_connection(connection)

  aoi_region_sets <- dplyr::tbl(con, "aoi_region_sets")

  if (is.null(connection)) {
    aoi_region_sets %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(aoi_region_sets)
}

#' Get AOI timepoints
#'
#' @inheritParams get_trials
#' @inheritParams get_administrations
#' @param rle Logical indicating whether to use RLE data representation or not
#'
#' @return A `tbl` of AOI Timepoints data, filtered down by supplied arguments.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
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
  ) %>%
    dplyr::collect()

  # if you are using the (default) RLE encoding, then get the RLE version
  # otherwise get the normal one.
  if (rle) {
    aoi_timepoints <- dplyr::tbl(con, "aoi_timepoints_rle")
  } else {
    aoi_timepoints <- dplyr::tbl(con, "aoi_timepoints")
  }

  # filter down to requested admins
  aoi_timepoints %<>%
    dplyr::filter(.data$administration_id %in%
                    !!administrations$administration_id)

  # collect the table locally
  aoi_timepoints %<>% dplyr::collect()
  DBI::dbDisconnect(con)

  # undo the RLE transform locally
  if (rle) {
    timestep <- 1000 / pkg_globals$SAMPLE_RATE

    aoi_timepoints %<>%
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

  return(aoi_timepoints)
}

#' Get XY timepoints
#'
#' @inheritParams get_trials
#' @inheritParams get_administrations
#'
#' @return A `tbl` of XY timepoints data, filtered down by supplied arguments.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_xy_timepoints(dataset_name = "reflook_v4")
#' }
get_xy_timepoints <- function(dataset_id = NULL, dataset_name = NULL,
                              age = NULL, connection = NULL) {
  con <- resolve_connection(connection)

  xy_timepoints <- dplyr::tbl(con, "xy_timepoints")

  administrations <- get_administrations(
    dataset_id = dataset_id,
    dataset_name = dataset_name,
    age = age, connection = con
  )

  xy_timepoints %<>%
    dplyr::semi_join(administrations, by = "administration_id")

  if (is.null(connection)) {
    xy_timepoints %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

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
    ungroup() |>
    pull(all_of(aux_name)) |>
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
    as_tibble() |>
    dplyr::mutate(across(everything(), \(aux) {
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
    dplyr::select(-all_of(aux_name)) |>
    tidyr::nest("{aux_name}" := all_of(colnames(aux_cols)))
}

#' Run a SQL Query script on the Peekbank database
#'
#' @param sql_query_string A valid sql query string character
#' @param connection A connection to the Peekbank database
#'
#' @return The database after calling the supplied SQL query
#' @export
#'
#' @examples
#' \dontrun{
#' get_sql_query("SELECT * FROM datasets")
#' }
get_sql_query <- function(sql_query_string, connection = NULL) {
  con <- resolve_connection(connection)
  if (is.null(con)) {
    return()
  }

  returned_sql_query <- dplyr::tbl(con, dplyr::sql(sql_query_string)) %>%
    dplyr::collect()
  if (is.null(connection)) {
    DBI::dbDisconnect(con)
  }
  return(returned_sql_query)
}


#' Download a list of files from OSF and recreate folder structure locally
#'
#' @param file_paths A character vector of file paths on OSF to download
#' @param osf_node_id The OSF node ID where the files are stored (default: "pr6wu")
#' @param local_base_dir Base directory to save files locally (default: here::here("data"))
#' @param debug Logical, whether to print debugging information (default: TRUE)
#' @param skip_existing Logical, skip downloading a file if a file with that name already exists in that path locally
#' @param max_retries Maximum number of retry attempts for server errors (default: 3)
#' @param retry_delay Delay in seconds between retry attempts (default: 5)
#'
#' @return returns paths to downloaded files
#'
#' @examples
#' \dontrun{
#' # Download multiple files from OSF
#' download_osf_files(
#'   file_paths = c(
#'     "lab1/raw_data/file1.csv",
#'     "lab2/processed_data/file2.csv"
#'   ),
#'   osf_node_id = "pr6wu"
#' )
#' }
download_osf_files <- function(file_paths, osf_node_id = "pr6wu", local_base_dir = "data",
                               debug = F, skip_existing = TRUE, max_retries = 3, retry_delay = 5) {
  if (!fs::dir_exists(local_base_dir)) {
    fs::dir_create(local_base_dir, recurse = TRUE)
  }

  downloaded_files <- character(length(file_paths))
  skipped_files <- character(0)

  # Cache for directory listings to avoid repeated API calls
  # Using an environment instead of a list for better indexing
  directory_cache <- new.env(hash = TRUE)

  get_all_items <- function(start_url, max_api_retries = max_retries, api_retry_delay = retry_delay) {
    if (exists(start_url, envir = directory_cache, inherits = FALSE)) {
      if (debug) message(glue::glue("Using cached data for: {start_url}"))
      return(get(start_url, envir = directory_cache))
    }

    all_names <- character(0)
    all_kinds <- character(0)
    all_related_hrefs <- character(0)
    all_downloads <- character(0)
    next_url <- start_url

    while (!is.null(next_url)) {
      if (debug) message(glue::glue("Fetching: {next_url}"))

      # Add retry logic for the GET request
      response <- NULL
      attempt <- 1
      success <- FALSE

      while (!success && attempt <= max_api_retries) {
        if (attempt > 1) {
          message(glue::glue("API retry attempt {attempt-1}/{max_api_retries} after waiting {api_retry_delay} seconds..."))
          Sys.sleep(api_retry_delay)
        }

        tryCatch({
          response <- httr::GET(next_url)
          status_code <- httr::status_code(response)

          if (status_code == 200) {
            success <- TRUE
          } else if (status_code >= 500 && status_code < 600 && attempt < max_api_retries) {
            message(glue::glue("Server error (HTTP {status_code}) when accessing OSF API. Will retry."))
          } else {
            # Other errors or final attempt
            if (attempt >= max_api_retries) {
              stop(glue::glue("Error accessing OSF API after {max_api_retries} attempts: {httr::content(response, 'text')}"))
            } else {
              message(glue::glue("HTTP error {status_code}. Will retry."))
            }
          }
        }, error = function(e) {
          if (attempt < max_api_retries) {
            message(glue::glue("Error when accessing OSF API: {e$message}. Will retry."))
          } else {
            stop(glue::glue("Failed to access OSF API after {max_api_retries} attempts: {e$message}"))
          }
        })

        attempt <- attempt + 1
      }

      # If we've reached here and success is TRUE, we have a valid response
      content <- jsonlite::fromJSON(httr::content(response, "text"))
      if (length(content$data) > 0) {
        all_names <- c(all_names, content$data$attributes$name)
        if ("kind" %in% names(content$data$attributes)) {
          all_kinds <- c(all_kinds, content$data$attributes$kind)
        } else {
          all_kinds <- c(all_kinds, rep(NA, length(content$data$attributes$name)))
        }

        if ("files" %in% names(content$data$relationships)) {
          all_related_hrefs <- c(all_related_hrefs, content$data$relationships$files$links$related$href)
        } else {
          all_related_hrefs <- c(all_related_hrefs, rep(NA, length(content$data$attributes$name)))
        }

        if ("download" %in% names(content$data$links)) {
          all_downloads <- c(all_downloads, content$data$links$download)
        } else {
          all_downloads <- c(all_downloads, rep(NA, length(content$data$attributes$name)))
        }
      }

      next_url <- NULL
      if ("next" %in% names(content$links) && !is.null(content$links[["next"]])) {
        next_url <- content$links[["next"]]
      }
    }

    result <- data.frame(
      name = all_names,
      kind = all_kinds,
      related_href = all_related_hrefs,
      download = all_downloads,
      stringsAsFactors = FALSE
    )

    assign(start_url, result, envir = directory_cache)
    return(result)
  }

  # Retry function for handling download errors
  download_with_retry <- function(url, destfile, max_attempts, delay_seconds) {
    attempt <- 1
    success <- FALSE

    while (!success && attempt <= max_attempts) {
      if (attempt > 1) {
        message(glue::glue("Retry attempt {attempt-1}/{max_attempts} after waiting {delay_seconds} seconds..."))
        Sys.sleep(delay_seconds)
      }

      tryCatch({
        curl::curl_download(url, destfile = destfile, quiet = FALSE)
        success <- TRUE
      }, error = function(e) {
        if (attempt < max_attempts) {
          if (grepl("HTTP error 5", e$message)) {
            message(glue::glue("Server error: {e$message}. Will retry."))
          } else {
            message(glue::glue("Error: {e$message}. Will retry."))
          }
        } else {
          message(glue::glue("Final attempt failed: {e$message}"))
          stop(e)
        }
      })

      attempt <- attempt + 1
    }

    return(success)
  }

  path_cache <- new.env(hash = TRUE)
  assign("ROOT", glue::glue("https://api.osf.io/v2/nodes/{osf_node_id}/files/osfstorage"), envir = path_cache)

  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    path_components <- fs::path_split(file_path)[[1]]
    file_name <- path_components[length(path_components)]
    dir_structure <- path_components[-length(path_components)]
    local_dir <- do.call(fs::path, c(list(local_base_dir), as.list(dir_structure)))
    local_file_path <- fs::path(local_dir, file_name)

    if (skip_existing && fs::file_exists(local_file_path)) {
      message(glue::glue("Skipping {file_path} - file already exists at {local_file_path}"))
      skipped_files <- c(skipped_files, local_file_path)
      downloaded_files[i] <- local_file_path
      next
    }

    if (!fs::dir_exists(local_dir)) {
      fs::dir_create(local_dir, recurse = TRUE)
    }

    current_path <- "ROOT"
    current_url <- get(current_path, envir = path_cache)
    # the sorting fixes the OSF bug that misses files otherwise
    current_url <- httr::modify_url(current_url, query = list(sort = "name"))

    for (component in dir_structure) {
      next_path <- if (current_path == "") component else fs::path(current_path, component)
      next_path_str <- as.character(next_path)

      if (exists(next_path_str, envir = path_cache, inherits = FALSE)) {
        if (debug) message(glue::glue("Using cached path for: {next_path_str}"))
        current_path <- next_path_str
        current_url <- get(current_path, envir = path_cache)
        next
      }

      items <- get_all_items(current_url)

      if (debug) {
        message("Available items at this level:")
        if (nrow(items) > 0) {
          for (j in seq_len(nrow(items))) {
            message(glue::glue("  - {items$name[j]} (type: {items$kind[j]})"))
          }
        } else {
          message("  No items found at this level")
        }
        message(glue::glue("Looking for: '{component}'"))
      }

      folder_idx <- which(items$name == component)
      if (length(folder_idx) == 0) {
        message(glue::glue("Error at: {file_path}"))
        stop(glue::glue("Could not find folder '{component}' in OSF path. Please check the path and try again."))
      }

      current_url <- items$related_href[folder_idx]
      current_path <- next_path_str
      assign(current_path, current_url, envir = path_cache)
    }

    # the sorting fixes the OSF bug that misses files otherwise
    current_url <- httr::modify_url(current_url, query = list(sort = "name"))
    items <- get_all_items(current_url)

    if (debug) {
      message("Available files in final directory:")
      if (nrow(items) > 0) {
        for (j in seq_len(nrow(items))) {
          message(glue::glue("  - {items$name[j]}"))
        }
      } else {
        message("No files found")
      }
      message(glue::glue("Looking for file: '{file_name}'"))
    }

    file_idx <- which(items$name == file_name)
    if (length(file_idx) == 0) {
      stop(glue::glue("Could not find file '{file_name}' in OSF path when processing {file_path}"))
    }

    download_url <- items$download[file_idx]
    message(glue::glue("Downloading {file_path} to {local_file_path}"))

    # Use our retry function instead of direct curl_download
    download_success <- download_with_retry(
      download_url,
      destfile = local_file_path,
      max_attempts = max_retries,
      delay_seconds = retry_delay
    )

    if (!download_success) {
      warning(glue::glue("Failed to download {file_path} after {max_retries} attempts"))
    } else {
      downloaded_files[i] <- local_file_path
    }
  }

  n_downloaded <- length(downloaded_files) - length(skipped_files)
  message(glue::glue("Downloaded {n_downloaded} files from OSF"))
  #if (length(skipped_files) > 0) {
  #  message(glue::glue("Skipped {length(skipped_files)} existing files"))
  #}

  return(downloaded_files)
}


#' Download stimulus images from OSF for Peekbank repository
#'
#' This function downloads stimulus images for selected Peekbank datasets from OSF.
#' It retrieves stimulus metadata from a Peekbank database connection, constructs
#' the full paths to the stimulus images on OSF, and downloads them to a local directory.
#'
#' @param con A database connection object created by connect_to_peekbank()
#' @param local_base_dir Local directory path where stimulus images will be saved (default: "stimulus_data")
#' @param datasets Character vector of dataset names to download stimuli for.
#'                 If empty (default), downloads stimuli for all datasets.
#' @param skip_existing skip downloading a file if a file with that name already exists in that path locally
#' @param debug show debug prints
#' @param max_retries Maximum number of retry attempts for server errors (default: 3)
#' @param retry_delay Delay in seconds between retry attempts (default: 5)
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
#' download_stimuli(con, local_base_dir = "stimulus_data", datasets = c("reflook_v4", "reflook_socword"))
#' }
#'
#' @export
download_stimuli <- function(con, local_base_dir = "stimulus_data", datasets = c(),
                             skip_existing=T, debug = F, max_retries = 3, retry_delay = 5) {
  stimuli_df <- get_stimuli(connection = con) %>%
    dplyr::collect() %>%
    dplyr::filter(!is.na(stimulus_image_path))

  if (length(datasets > 0)) {
    stimuli_df <- stimuli_df %>% dplyr::filter(dataset_name %in% datasets)
  }

  paths <- stimuli_df %>%
    dplyr::mutate(full_stimulus_path = paste0(dataset_name, "/raw_data/", stimulus_image_path)) %>%
    dplyr::pull(full_stimulus_path) %>%
    download_osf_files(local_base_dir = local_base_dir, skip_existing = skip_existing, debug = debug,
                       max_retries = max_retries, retry_delay = retry_delay)


  return(stimuli_df %>% dplyr::mutate(local_stimulus_path = paths))
}
