#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

#' Connect to the peekbank database
#'
#' @return A connection to the peekbank database.
#' @export
#'
#' @examples
#' con <- connect_to_peekbank()
#' DBI::dbDisconnect(con)
connect_to_peekbank <- function() {

  DBI::dbConnect(RMySQL::MySQL(),
                 host = "ec2-18-237-23-48.us-west-2.compute.amazonaws.com",
                 dbname = "peekbank",
                 user = "root", password = "NpwvcxhF249PdESheFqS")

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
#' con <- connect_to_peekbank()
#' list_peekbank_tables(con)
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
#' get_datasets()
get_datasets <- function(connection = NULL) {
  con <- resolve_connection(connection)

  datasets <- dplyr::tbl(con, "datasets")

  if (is.null(connection)) {
    datasets %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(datasets)

}

#' Get subjects
#'
#' @param age A numeric vector of a single age or a min age and max age
#'   (inclusive), in months
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Subjects data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_subjects()
#' get_subjects(age = c())
get_subjects <- function(age = NULL, connection = NULL) {
  con <- resolve_connection(connection)
  input_age <- age

  subjects <- dplyr::tbl(con, "subjects")

  if (!is.null(input_age)) {
    if (length(input_age) == 1) {
      subjects %<>% dplyr::filter(age == input_age)
    } else if (length(input_age) == 2) {
      min_age <- input_age[1]
      max_age <- input_age[2]
      subjects %<>% dplyr::filter(age >= min_age & age <= max_age)
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  if (is.null(connection)) {
    subjects %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(subjects)

}

#' Get trials
#'
#' @param dataset_id An integer vector of one or more dataset ids
#' @param lab_dataset_id A character vector of one or more lab dataset ids
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Trials data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_trials()
#' get_trials(lab_dataset_id = "pomper_saffran_2016")
get_trials <- function(dataset_id = NULL, lab_dataset_id = NULL, connection = NULL) {
  con <- resolve_connection(connection)
  input_dataset_id <- dataset_id
  input_lab_dataset_id <- lab_dataset_id

  trials <- dplyr::tbl(con, "trials")

  datasets <- dplyr::tbl(con, "datasets")
  if (!is.null(dataset_id)) datasets %<>%
    dplyr::filter(.data$dataset_id %in% input_dataset_id)
  if (!is.null(lab_dataset_id)) datasets %<>%
    dplyr::filter(lab_dataset_id %in% input_lab_dataset_id)
  num_datasets <- datasets %>% dplyr::tally() %>% dplyr::pull(.data$n)
  if (num_datasets == 0) stop("No matching datasets found")

  trials %<>% dplyr::inner_join(datasets, by = "dataset_id")

  if (is.null(connection)) {
    trials %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(trials)

}

#' Get AOI regions
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of AOI Regions data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_aoi_regions()
get_aoi_regions <- function(connection = NULL) {
  con <- resolve_connection(connection)

  aoi_regions <- dplyr::tbl(con, "aoi_regions")

  if (is.null(connection)) {
    aoi_regions %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(aoi_regions)

}

#' Get AOI data
#'
#' @inheritParams get_trials
#' @inheritParams get_subjects
#'
#' @return A `tbl` of AOI data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_aoi_data(lab_dataset_id = "pomper_saffran_2016")
get_aoi_data <- function(dataset_id = NULL, lab_dataset_id = NULL, age = NULL,
                         connection = NULL) {
  con <- resolve_connection(connection)

  aoi_data <- dplyr::tbl(con, "aoi_data")

  trials <- get_trials(dataset_id, lab_dataset_id, con)
  aoi_data %<>% dplyr::inner_join(trials, by = "trial_id")

  subjects <- get_subjects(age, con)
  aoi_data %<>% dplyr::inner_join(subjects, by = "subject_id")

  if (is.null(connection)) {
    aoi_data %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(aoi_data)

}

#' Get XY data
#'
#' @inheritParams get_trials
#' @inheritParams get_subjects
#'
#' @return A `tbl` of XY data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_xy_data(lab_dataset_id = "pomper_saffran_2016")
get_xy_data <- function(dataset_id = NULL, lab_dataset_id = NULL, age = NULL,
                        connection = NULL) {
  con <- resolve_connection(connection)

  xy_data <- dplyr::tbl(con, "xy_data")

  trials <- get_trials(dataset_id, lab_dataset_id, con)
  xy_data %<>% dplyr::inner_join(trials, by = "trial_id")

  subjects <- get_subjects(age, con)
  xy_data %<>% dplyr::inner_join(subjects, by = "subject_id")

  if (is.null(connection)) {
    xy_data %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(xy_data)

}
