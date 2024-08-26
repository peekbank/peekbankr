#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

options(warn=-1)

pkg_globals <- new.env()
pkg_globals$SAMPLE_RATE <- 40 # Hz

translate_version <- function(db_version, db_args, db_info) {

  # using the peekbankr hosted server
  if (db_args$host == db_info$host) {

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
      stop("Version '", db_version, "' is no longer hosted by ",
           "peekbank.stanford.edu; either specify a more recent version or ",
           "install MySQL Server locally and update db_args.")

      # version not recognized
    } else {
      stop("Version '", db_version, "' not found. Specify one of: 'current', ",
           paste(sprintf("'%s'", db_info$supported), collapse = ", "), ".")
    }

    # using a different server than the peekbankr hosted one
  } else {
    message("Not using hosted database version; no checks will be applied to ",
            "version specification.")
    return(db_args$db_name)
  }
}

resolve_connection <- function(connection, db_version = NULL, db_args = NULL) {
  if (is.null(connection)) connect_to_peekbankr(db_version, db_args)
  else connection
}

#' Get information on database connection options
#'
#' @return List of database info: host name, current version, supported
#'   versions, historical versions, username, password
#' @export
#'
#' @examples
#' \donttest{
#' get_db_info()
#' }
get_db_info <- function() {
  jsonlite::fromJSON("https://langcog.github.io/peekbank-website/peekbank.json")
}

#' Connect to Peekbank
#'
#' @param db_version String of the name of database version to use
#' @param db_args List with host, user, and password defined
#' @param compress Flag to use compression protocol (defaults to TRUE)
#'
#' @return con A DBIConnection object for the peekbank database
#' @export
#'
#' @examples
#' \donttest{
#' con <- connect_to_peekbank(db_version = "current", db_args = NULL)
#' DBI::dbDisconnect(con)
#' }
connect_to_peekbank <- function(db_version = "current", db_args = NULL,
                                compress = TRUE) {

  db_info <- get_db_info()

  flags <- if (compress) RMariaDB::CLIENT_COMPRESS else 0

  if (is.null(db_args)) db_args <- db_info


  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = db_args$host,
    dbname = translate_version(db_version, db_args, db_info),
    user = db_args$user,
    password = db_args$password,
    client.flag = flags
  )
}

resolve_connection <- function(connection) {
  if (is.null(connection)) connect_to_peekbank() else connection
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
          seq(
            td$t_norm[1], td$t_norm[1] + (sum(td$length) - 1) * timestep,
            timestep
          )
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
#' get_xy_timepoints(dataset_name = "pomper_saffran_2016")
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
  if (length(aux_name) == 0) return(df)
  aux_list <- df |>
    ungroup() |>
    pull(all_of(aux_name)) |>
    lapply(\(aux) {
      if(is.na(aux) | is.null(aux)) return(aux)
      jsonlite::fromJSON(aux)
    })
  if(all(is.na(aux_list))) return(df)

  col_names <- purrr::flatten(aux_list) |> names() |> unique()
  col_names <- col_names[!is.na(col_names) & col_names != ""]

  aux_cols <- lapply(col_names, \(col_name) {
    sapply(aux_list, \(aux) {
      aux[col_name]
      })
  }) |>
    `names<-`(value = col_names) |>
    as_tibble() |>
    mutate(across(everything(), \(aux) {
      if (any(sapply(aux, \(aux_val) {typeof(aux_val) == "list"}))) {
        aux <- lapply(aux, \(aux_val) {
          if(all(is.na(aux_val))) return(NA)
          bind_rows(aux_val)
        })}
      if (all(sapply(aux, is.atomic))) {
        aux <- list_simplify(aux, strict = FALSE) # May need a better fix for NAs
      }
      aux
    }))
  df |>
    cbind(aux_cols) |>
    select(-all_of(aux_name)) |>
    nest("{aux_name}" := all_of(colnames(aux_cols)))
}
