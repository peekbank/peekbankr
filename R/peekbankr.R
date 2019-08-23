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
#' @return A `tbl` of Datasets data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
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
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Subjects data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_subjects()
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
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of Trials data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_trials()
get_trials <- function(connection = NULL) {
  con <- resolve_connection(connection)

  trials <- dplyr::tbl(con, "trials")

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
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of AOI data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_aoi_data
get_aoi_data <- function(connection = NULL) {
  con <- resolve_connection(connection)

  aoi_data <- dplyr::tbl(con, "aoi_data")

  if (is.null(connection)) {
    aoi_data %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(aoi_data)

}

#' Get XY data
#'
#' @inheritParams list_peekbank_tables
#'
#' @return A `tbl` of XY data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' get_xy_data()
get_xy_data <- function(connection = NULL) {
  con <- resolve_connection(connection)

  xy_data <- dplyr::tbl(con, "xy_data")

  if (is.null(connection)) {
    xy_data %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(xy_data)

}
