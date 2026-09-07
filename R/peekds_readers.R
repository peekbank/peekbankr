#' parse the bundled peekbank schema json into a dataframe
#'
#' @return the organized dataframe from schema json file
#'
#' @examples
#' \dontrun{
#' peekjson <-ds.get_peekjson()
#' }
#'
#' @export
ds.get_peekjson <- function() {
  peekjson <- jsonlite::fromJSON(system.file(pkg_globals$SCHEMA_FILE, package = "peekbankr"))
  return(peekjson)
}

#' Fetching the list of field names and requirements in each table according to
#' the schema json file
#'
#' @param table_type the type of dataframe, for the most updated table types
#'   specified by schema, please use functionds.list_ds_tables()
#'
#' @return the list of field names
#'
#' @examples
#' \dontrun{
#' fields_json <-ds.get_json_fields(table_type = "aoi_timepoints")
#' }
#'
#' @export
ds.get_json_fields <- function(table_type) {
  # get json file from github
  peekjson <-ds.get_peekjson()
  # fetch the table listb =
  table_list <- as.vector(peekjson[, "table"])

  # check if the input table_type is valid
  if (!(table_type %in% table_list)) {
    stop(.msg("Cannot recognize the table type {table_type}."))
  }

  # get the list of column names in json
  fields_json <- peekjson[which(peekjson$table == table_type), "fields"]
  fields_json <- fields_json[[1]]

  return(fields_json)
}

#' List the tables required based on coding method
#'
#' @param coding_methods a list of strings indicating the methods used in the experiment for
#'   coding gaze data, to get the list of current coding methods, please use
#'   functionds.list_coding_methods()
#'
#' @return a list of table types that are required based on input coding method
#'
#' @examples
#' \dontrun{
#' table_list <-ds.list_ds_tables(coding_methods = "manual gaze coding")
#' }
#'
#' @export
ds.list_ds_tables <- function(coding_methods = c("eyetracking")) {
  # get json file from github
  peekjson <-ds.get_peekjson()
  # get the list of tables other than admin
  table_list <- peekjson$table
  table_list_auto <- table_list[table_list != "admin"]

  table_list_manual <- table_list_auto[!(table_list_auto %in%
                                           c("xy_timepoints",
                                             "aoi_region_sets"))]

  if(!all(coding_methods %in% c("eyetracking", "automated gaze coding", "manual gaze coding", "preprocessed eyetracking"))){
    stop(.msg("Invalid coding method type! The type can only be one of the
              following: {paste0(ds_list_coding_methods(), collapse = ', ')}."))
  }


  # if any are eyetracking or automated gaze coding data, require all tables
  if (length(intersect(coding_methods, c("eyetracking", "automated gaze coding")))) {
    table_list <- table_list_auto
  } else if (length(intersect(coding_methods, c("manual gaze coding",
                                  "preprocessed eyetracking")))) {
    table_list <- table_list_manual
  } else {
    stop(.msg("Invalid coding method type! The type can only be one of the
              following: {paste0(ds_list_coding_methods(), collapse = ', ')}."))
  }
  return(table_list)
}

#' Get the coding method list from json schema file
#'
#' @return a list of strings indicating allowed coding methods
#'
#' @examples
#' \dontrun{
#' coding_methods <-ds.list_coding_methods()
#' }
#' @export
ds.list_coding_methods <- function() {
  fields_json <-ds.get_json_fields(table_type = "administrations")
  methods_json <- fields_json$options[fields_json$field_name == "coding_method",
                                      "choices"] %>%
    unlist() %>%
    unique()
  return(methods_json)
}

#' Check if a certain table is required according to schema
#'
#' @param table_type the type of dataframe, for the most updated table types
#'   specified by schema, please use functionds.list_ds_tables()
#' @param coding_methods methods used in the experiment for coding gaze data, to
#'   get the list of current coding methods, please use function
#'  ds.list_coding_methods()
#'
#' @return A boolean value
#'
#' @examples
#' \dontrun{
#' is_required <-ds.is_table_required(table_type = "xy_timepoints",
#'                                  coding_method = "manual gaze coding")
#' }
#' @export
ds.is_table_required <- function(table_type, coding_methods) {
  table_list <-ds.list_ds_tables(coding_methods)
  is_required <- table_type %in% table_list
  return(is_required)
}

#' List current allowed language choices for db import
#'
#' @return a list of strings containing all the allowed language codes based on
#'   json schema file
#'
#' @examples
#' \dontrun{
#' language_list <-ds.list_language_choices()
#' }
#'
#' @export
ds.list_language_choices <- function() {
  # get json file from github
  fields_json <-ds.get_json_fields(table_type = "trial_types")
  idx <- match("full_phrase_language", fields_json$field_name)
  fieldoptions <- fields_json$options[idx, ]
  language_list <- unique(unlist(fieldoptions$choices))

  return(language_list)
}

#' Function for mapping raw data columns to processed table columns
#'
#' @param raw_data raw data frame
#' @param raw_format source of the eye-tracking data, e.g. "tobii"
#' @param table_type type of processed table, e.g. "xy_data" | "aoi_table"
#'
#' @return processed data frame with specified column names
#'
#' @examples
#' \dontrun{
#' df_xy_data <-ds.map_columns(raw_data = raw_data, raw_format = "tobii",
#'                           table_type = "xy_data")
#' df_aoi_data <-ds.map_columns(raw_data = raw_data, raw_format = "tobii",
#'                            table_type = "aoi_data")
#' }
#'
#' @export
ds.map_columns <- function(raw_data, raw_format, table_type) {
  # dir_mapping won't be an input parameter, instead the function by default
  # assumes that this file should be in import_scripts/import_header.csv
  file_header <- "import_header.csv"
  if (!file.exists(file_header)) {
    stop(.msg("'import_header.csv' file is required for calling theds.map_columns
              function."))
  }

  ## STEP 0. read in raw datafiles and the column names
  df_header <- utils::read.csv(file = file_header, header = TRUE, sep = ",")
  df_map <- df_header[
    which((df_header$format == raw_format) & (df_header$table == table_type)), ]
  if (nrow(df_map) == 0) {
    stop(.msg("User did not provide mapping columns between raw data and table
              {table_type} in 'import_scripts/import_header.csv'. Thus
              {table_type} table is not processed."))
  }
  colnames_raw <- colnames(raw_data)

  colnames_fetch <- as.vector(df_map[, "raw_column"])
  colnames_map <- as.vector(df_map[, "mapped_column"])

  ## create new data table with NA values
  df_table <- data.frame(
    matrix(
      ncol = length(colnames_map), nrow = nrow(raw_data)))
  colnames(df_table) <- colnames_map

  ## search through raw data table and find desired columns,
  ## if they did not exist, then just the column will be left with NA values
  for (i in 1:length(colnames_fetch)) {
    if (colnames_fetch[i] %in% colnames_raw) {
      df_table[i] <- raw_data[colnames_fetch[i]]
    } else if (colnames_fetch[i] == "") {
      stop(.msg("The raw data column to be mapped to {colnames_map[i]} is not
                specified."))
    }
    else {
      stop(.msg("Cannot find column {colnames_fetch[i]} in {raw_format} raw data
                file."))
    }
  }

  return(df_table)
}
