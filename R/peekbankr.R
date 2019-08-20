#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
NULL

# utils::globalVariables(c("collection_id", "collection_name", "corpus_id",
#                          "corpus_name", "gloss", "id", "max_age", "min_age",
#                          "name", "speaker_role", "target_child_id",
#                          "target_child_name", "target_child_age",
#                          "utterance_id", "transcript_id", "utterance_order",
#                          "replacement"))

avg_month <- 365.2425 / 12

translate_version <- function(db_version, db_args, db_info) {

  # using the childes-db hosted server
  if (db_args$host == db_info$host) {

    # current version
    if (db_version == "current") {
      db_to_use <- db_info[["current"]]
      message("Using current database version: '", db_to_use, "'.")
      return("childesdb")

      # supported version
    } else if (db_version %in% db_info[["supported"]]) {
      db_to_use <- db_version
      message("Using supported database version: '", db_to_use, "'.")
      return("childesdb")

      # historical version
    } else if (db_version %in% db_info[["historical"]]) {
      stop("Version '", db_version, "' is no longer hosted by ",
           "childes-db.stanford.edu; either specify a more recent version or ",
           "install MySQL Server locally and update db_args.")

      # version not recognized
    } else {
      stop("Version '", db_version, "' not found. Specify one of: 'current', ",
           paste(sprintf("'%s'", db_info$supported), collapse = ", "), ".")
    }

    # using a different server than the childes-db hosted one
  } else {
    message("Not using hosted database version; no checks will be applied to ",
            "version specification.")
    return(db_args$db_name)
  }
}

resolve_connection <- function(connection, db_version = NULL, db_args = NULL) {
  if (is.null(connection)) connect_to_peekbank(db_version, db_args)
  else connection
}

#' Connect to peekbank
#'
#' @param db_version String of the name of database version to use
#' @param db_args List with host, user, and password defined
#' @return con A DBIConnection object for the peekbank database
#' @export
#'
#' @examples
#' \donttest{
#' con <- connect_to_peekbank(db_version = "current", db_args = NULL)
#' DBI::dbDisconnect(con)
#' }
connect_to_peekbank <- function(db_version = "current", db_args = NULL) {
  # TODO: what does this config file contain and where to put it?
  # db_info <- jsonlite::fromJSON(
  #   "https://peekbank.stanford.edu/peekbank.json"
  # )

  if (is.null(db_args)) db_args <- db_info

  con <- DBI::dbConnect(
    RMySQL::MySQL(),
    host = db_args$host,
    dbname = translate_version(db_version, db_args, db_info),
    user = db_args$user,
    password = db_args$password
  )
  DBI::dbGetQuery(con, "SET NAMES utf8")
  return(con)
}
