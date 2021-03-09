#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

utils::globalVariables(c("trial_type_id"))
pkg_globals <- new.env()
pkg_globals$SAMPLE_RATE <- 40 # Hz


#' Connect to the peekbank database
#'
#' @param host Database connection host
#' @param dbname Database connection db name
#' @param user Database connection user
#' @param password Database connection password
#' @param compress Flag to use compression protocol (defaults to TRUE)
#'
#' @return A connection to the peekbank database.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_peekbank()
#' DBI::dbDisconnect(con)
#' }
connect_to_peekbank <- function(host = "34.210.173.143",
                                dbname = "peekbank",
                                user = "reader",
                                password = "gazeofraccoons",
                                compress = TRUE) {

  flags <- if (compress) RMySQL::CLIENT_COMPRESS else 0
  DBI::dbConnect(RMySQL::MySQL(),
                 host = host, dbname = dbname,
                 user = user, password = password,
                 client.flag = flags)
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
  datasets %>% dplyr::collect() %>% dplyr::tally() %>% dplyr::pull(.data$n)
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

  if (!is.null(dataset_id)) datasets %<>%
    dplyr::filter(.data$dataset_id %in% input_dataset_id)
  if (!is.null(dataset_name)) datasets %<>%
    dplyr::filter(dataset_name %in% input_dataset_name)

  num_datasets <- count_datasets(datasets)

  if (num_datasets == 0) stop("No matching datasets found")

  if (!is.null(input_age)) {
    if (length(input_age) == 1) {
      administrations %<>% dplyr::filter(age == input_age)
    } else if (length(input_age) == 2) {
      min_age <- input_age[1]
      max_age <- input_age[2]
      administrations %<>% dplyr::filter(age >= min_age & age <= max_age)
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  datasets %<>% dplyr::select(dataset_id, dataset_name)
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
  if (!is.null(dataset_id)) datasets %<>%
    dplyr::filter(.data$dataset_id %in% input_dataset_id)
  if (!is.null(dataset_name)) datasets %<>%
    dplyr::filter(dataset_name %in% input_dataset_name)
  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  trial_types %<>% dplyr::select(trial_type_id, dataset_id)
  datasets %<>% dplyr::select(dataset_id, dataset_name)
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
  if (!is.null(dataset_id)) datasets %<>%
    dplyr::filter(.data$dataset_id %in% input_dataset_id)
  if (!is.null(dataset_name)) datasets %<>%
    dplyr::filter(dataset_name %in% input_dataset_name)
  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  datasets %<>% dplyr::select(dataset_id, dataset_name)
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
  if (!is.null(dataset_id)) datasets %<>%
    dplyr::filter(.data$dataset_id %in% input_dataset_id)
  if (!is.null(dataset_name)) datasets %<>%
    dplyr::filter(dataset_name %in% input_dataset_name)
  num_datasets <- count_datasets(datasets)
  if (num_datasets == 0) stop("No matching datasets found")

  datasets %<>% dplyr::select(dataset_id, dataset_name)
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

#' Get AOI timepoints. Note that by default, under the hood, we are getting
#' the aoi_timepoints_rle table, which uses run length encoding for compression,
#' and then undoing this transform. (This compression should be transparent to
#' the end user).
#'
#' @inheritParams get_trials
#' @inheritParams get_administrations
#'
#' @return A `tbl` of AOI Timepoints data, filtered down by supplied arguments.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' get_aoi_timepoints(dataset_name = "pomper_saffran_2016")
#' get_aoi_timepoints(dataset_name = "pomper_saffran_2016", age = c())
get_aoi_timepoints <- function(dataset_id = NULL, dataset_name = NULL, age = NULL,
                         connection = NULL, rle = TRUE) {

  # if (rle & !is.null(connection)) {
  #   error("Lazy remote queries åre not supported with the RLE table, use rle = FALSE.")
  # }

  con <- resolve_connection(connection)

  administrations <- get_administrations(age = age, dataset_id = dataset_id,
                                         dataset_name = dataset_name,
                                         connection = con) %>%
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
    dplyr::filter(administration_id %in% !!administrations$administration_id)

  # collect the table locally
  # if (is.null(connection)) {
    aoi_timepoints %<>%
      dplyr::collect()
    DBI::dbDisconnect(con)
  # }

  # undo the RLE transform locally
  if (rle) {
    timestep <- 1000/pkg_globals$SAMPLE_RATE

    aoi_timepoints %<>%
      dplyr::group_by(administration_id, trial_id) %>%
      tidyr::nest() %>%
      dplyr::mutate(rle_vector = map(data,
                              ~ `class<-`(list(lengths = .$length, values = .$aoi), "rle")),
             aoi = purrr::map(rle_vector, inverse.rle),
             t_norm = map(data,
                          ~ seq(.$t_norm[1], .$t_norm[1] + (sum(.$length)-1)*timestep,
                                timestep))) %>%
      dplyr::select(-data, -rle_vector) %>%
      tidyr::unnest(cols = c(aoi, t_norm)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(aoi_timepoint_id = 1:n())
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

  administrations <- get_administrations(dataset_id = dataset_id,
                                         dataset_name = dataset_name,
                                         age = age, connection = con)

  xy_timepoints %<>%
    dplyr::semi_join(administrations, by = "administration_id")

  if (is.null(connection)) {
    xy_timepoints %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(xy_timepoints)

}
