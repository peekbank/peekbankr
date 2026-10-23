# The interface before the client: bare functions that each take a
# `connection`. These are superseded but still work in order to not break old analysis scripts.

validate_dataset_args <- function(dataset_id, dataset_name) {
  if (!is.null(dataset_id) && !is.numeric(dataset_id)) {
    stop("dataset_id must be numeric. Did you mean to use dataset_name = \"", dataset_id, "\"?")
  }
  if (!is.null(dataset_name) && !is.character(dataset_name)) {
    stop("dataset_name must be a character string.")
  }
}

#' Connect to Peekbank
#'
#' @description
#' Superseded. Use [peekbank()], which returns a client that pins a release
#' for a whole analysis.
#'
#' @details
#' `connect_to_peekbank()` returns a handle that pins a database version (Redivis
#' dataset version)
#'
#' @param db_version String naming the database version to use: a Peekbank
#'    release name (e.g. "2025.1"), or "current" (the default) for whichever
#'    version was published most recently
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
  new_connection(db_version)
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
#' @description
#' Superseded. Use `pb$tables` on a [peekbank()] client.
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
  fetch_table_names(resolve_connection(connection))
}

#' Get datasets
#'
#' @description
#' Superseded. Use `pb$datasets` on a [peekbank()] client.
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
    dplyr::tally() %>%
    dplyr::pull(.data$n)
}

# The dataset ids these arguments select, or NULL when they select everything.
# Stops when nothing matches, so that a misspelled dataset is distinguishable
# from one that happens to have no rows.
matching_dataset_ids <- function(con, dataset_id = NULL, dataset_name = NULL) {
  datasets <- pb_table(con, "datasets")
  if (!is.null(dataset_id)) {
    datasets <- dplyr::filter(datasets,
                              .data$dataset_id %in% .env$dataset_id)
  }
  if (!is.null(dataset_name)) {
    datasets <- dplyr::filter(datasets,
                              .data$dataset_name %in% .env$dataset_name)
  }
  if (count_datasets(datasets) == 0) stop("No matching datasets found")
  if (is.null(dataset_id) && is.null(dataset_name)) return(NULL)
  datasets$dataset_id
}

restrict_to_datasets <- function(x, keep) {
  if (is.null(keep)) return(x)
  dplyr::filter(x, .data$dataset_id %in% .env$keep)
}

filter_age <- function(administrations, age) {
  if (is.null(age)) return(administrations)
  if (length(age) == 1) {
    dplyr::filter(administrations, .data$age == .env$age)
  } else if (length(age) == 2) {
    dplyr::filter(administrations,
                  .data$age >= .env$age[1], .data$age <= .env$age[2])
  } else {
    stop("`age` argument must be of length 1 or 2")
  }
}

# No ids gives FALSE, because `IN ()` will not parse. Ids arrive as doubles,
# which R would otherwise render in scientific notation (hmmm).
sql_in_clause <- function(column, ids) {
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) return("FALSE")
  sprintf("%s IN (%s)", column,
          paste(format(ids, scientific = FALSE, trim = TRUE), collapse = ", "))
}

#' Get administrations
#'
#' @description
#' Superseded. Use `pb$administrations`, or
#' `pb$subset$administrations()` to narrow it, on a [peekbank()] client.
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
  keep <- matching_dataset_ids(con, dataset_id, dataset_name)
  fetch_table(con, "administrations") %>%
    restrict_to_datasets(keep) %>%
    filter_age(age)
}

#' Get subjects
#'
#' @description
#' Superseded. Use `pb$subjects` on a [peekbank()] client.
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
#' @description
#' Superseded. Use `pb$trials` or `pb$subset$trials()` on a [peekbank()]
#' client.
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
  keep <- matching_dataset_ids(con, dataset_id, dataset_name)
  restrict_to_datasets(fetch_table(con, "trials"), keep)
}

#' Get trial types
#'
#' @description
#' Superseded. Use `pb$trial_types` or `pb$subset$trial_types()` on a
#' [peekbank()] client.
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
  keep <- matching_dataset_ids(con, dataset_id, dataset_name)
  restrict_to_datasets(fetch_table(con, "trial_types"), keep)
}

#' Get stimuli
#'
#' @description
#' Superseded. Use `pb$stimuli` or `pb$subset$stimuli()` on a [peekbank()]
#' client.
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
  keep <- matching_dataset_ids(con, dataset_id, dataset_name)
  restrict_to_datasets(fetch_table(con, "stimuli"), keep)
}

#' Get AOI region sets
#'
#' @description
#' Superseded. Use `pb$aoi_region_sets` on a [peekbank()] client.
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
#' @description
#' Superseded. Use `pb$aoi_timepoints`, or
#' `pb$subset$aoi_timepoints()` to filter on the server, on a [peekbank()]
#' client.
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
      sql_in_clause("administration_id", administrations$administration_id))
  } else {
    sql <- sprintf(
      "SELECT * FROM aoi_timepoints WHERE %s ORDER BY aoi_timepoint_id",
      sql_in_clause("administration_id", administrations$administration_id))
  }
  aoi_timepoints <- pb_query(con, sql)
  if (is.null(aoi_timepoints)) return(NULL)

  # undo the RLE transform locally
  if (rle) {
    aoi_timepoints <- decode_rle_timepoints(aoi_timepoints)
  }

  return(aoi_timepoints)
}

#' Get XY timepoints
#'
#' @description
#' Superseded. Use `pb$xy_timepoints` or `pb$subset$xy_timepoints()` on a
#' [peekbank()] client.
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
    sql_in_clause("administration_id", administrations$administration_id))
  xy_timepoints <- pb_query(con, sql)

  return(xy_timepoints)
}

#' Run a SQL query against peekbank
#'
#' @description
#' Superseded. Use `pb$query()` on a [peekbank()] client, or `pb$db$<table>`
#' for a dbplyr handle.
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
#' @description
#' Superseded. Use `pb$images()` on a [peekbank()] client.
#'
#' This function downloads stimulus images for selected Peekbank datasets from
#' Redivis. It retrieves stimulus metadata from a
#' Peekbank database connection, constructs the full stimulus file paths, and
#' downloads them to a local directory.
#'
#' @param con A version handle created by connect_to_peekbank()
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
download_stimuli <- function(con, local_base_dir = "stimulus_data",
                             datasets = c(), skip_existing = TRUE) {
  fetch_stimulus_images(con, datasets = datasets, dir = local_base_dir,
                        skip_existing = skip_existing) %>%
    dplyr::rename(local_stimulus_path = "local_image_path")
}

#' Download dataset README files
#'
#' @description
#' Superseded. Use `pb$readmes()` on a [peekbank()] client.
#'
#' Downloads README files for Peekbank datasets from Redivis.
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
get_readmes <- function(datasets = c(),
                        local_base_dir = "dataset_readmes") {
  download_readmes(if (length(datasets) > 0) datasets, local_base_dir)
  invisible(NULL)
}

#' Get information on database connection options
#'
#' @description
#' Superseded. Use [peekbank_releases()].
#'
#' @details
#' Data are retrieved from the versioned peekbank
#' dataset on Redivis. Version information is discovered from the
#' `release_info` table naming the Peekbank release it holds.
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
  releases <- peekbank_releases()
  if (is.null(releases)) return(NULL)
  list(
    current = releases$release[releases$current],
    versions = stats::setNames(releases$redivis_tag, releases$release)
  )
}
