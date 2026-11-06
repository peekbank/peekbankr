#' parse the bundled peekbank schema json into a dataframe
#'
#' @return the organized dataframe from schema json file
#'
#' @examples
#' \dontrun{
#' peekjson <- ds.get_peekjson()
#' }
#'
#' @export
ds.get_peekjson <- function() {
  peekjson <- jsonlite::fromJSON(system.file("peekbank-schema.json", package = "peekbankr"))
  return(peekjson)
}

#' Fetching the list of field names and requirements in each table according to
#' the schema json file
#'
#' @param table_type the type of dataframe, for the most updated table types
#'   specified by schema, please use ds.list_ds_tables()
#'
#' @return the list of field names
#'
#' @examples
#' \dontrun{
#' fields_json <- ds.get_json_fields(table_type = "aoi_timepoints")
#' }
#'
#' @export
ds.get_json_fields <- function(table_type) {
  # get json file from github
  peekjson <- ds.get_peekjson()
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
#'   ds.list_coding_methods()
#'
#' @return a list of table types that are required based on input coding method
#'
#' @examples
#' \dontrun{
#' table_list <- ds.list_ds_tables(coding_methods = "manual gaze coding")
#' }
#'
#' @export
ds.list_ds_tables <- function(coding_methods = c("eyetracking")) {
  peekjson <- ds.get_peekjson()
  table_list_auto <- peekjson$table

  table_list_manual <- table_list_auto[!(table_list_auto %in%
                                           c("xy_timepoints",
                                             "aoi_region_sets"))]

  if(!all(coding_methods %in% c("eyetracking", "automated gaze coding", "manual gaze coding", "preprocessed eyetracking"))){
    stop(.msg("Invalid coding method type! The type can only be one of the
              following: {paste0(ds.list_coding_methods(), collapse = ', ')}."))
  }


  # if any are eyetracking or automated gaze coding data, require all tables
  if (length(intersect(coding_methods, c("eyetracking", "automated gaze coding")))) {
    table_list <- table_list_auto
  } else if (length(intersect(coding_methods, c("manual gaze coding",
                                  "preprocessed eyetracking")))) {
    table_list <- table_list_manual
  } else {
    stop(.msg("Invalid coding method type! The type can only be one of the
              following: {paste0(ds.list_coding_methods(), collapse = ', ')}."))
  }
  return(table_list)
}

#' Get the coding method list from json schema file
#'
#' @return a list of strings indicating allowed coding methods
#'
#' @examples
#' \dontrun{
#' coding_methods <- ds.list_coding_methods()
#' }
#' @export
ds.list_coding_methods <- function() {
  fields_json <- ds.get_json_fields(table_type = "administrations")
  methods_json <- fields_json$options[fields_json$field_name == "coding_method",
                                      "choices"] %>%
    unlist() %>%
    unique()
  return(methods_json)
}

#' Check if a certain table is required according to schema
#'
#' @param table_type the type of dataframe, for the most updated table types
#'   specified by schema, please use ds.list_ds_tables()
#' @param coding_methods methods used in the experiment for coding gaze data, to
#'   get the list of current coding methods, please use function
#'  ds.list_coding_methods()
#'
#' @return A boolean value
#'
#' @examples
#' \dontrun{
#' is_required <- ds.is_table_required(table_type = "xy_timepoints",
#'                                  coding_method = "manual gaze coding")
#' }
#' @export
ds.is_table_required <- function(table_type, coding_methods) {
  table_list <- ds.list_ds_tables(coding_methods)
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
#' language_list <- ds.list_language_choices()
#' }
#'
#' @export
ds.list_language_choices <- function() {
  # get json file from github
  fields_json <- ds.get_json_fields(table_type = "trial_types")
  idx <- match("full_phrase_language", fields_json$field_name)
  fieldoptions <- fields_json$options[idx, ]
  language_list <- unique(unlist(fieldoptions$choices))

  return(language_list)
}

# The language names CDI scores and lang_measures in subjects' aux data must
# use, as Wordbank spells them. Trial languages are ISO codes from the schema,
# see ds.list_language_choices().
WORDBANK_ALLOWED_LANGUAGES <- c(
  "Danish",
  "English (American)",
  "English (Australian)",
  "English (British)",
  "French (French)",
  "French (Quebecois)",
  "German",
  "Italian",
  "Greek (Cypriot)",
  "Norwegian",
  "Portuguese (European)",
  "Russian",
  "Spanish (European)",
  "Spanish (Mexican)",
  "Swedish",
  "Turkish",
  "Croatian",
  "Czech",
  "Mandarin (Beijing)",
  "Mandarin (Taiwanese)",
  "Cantonese",
  "Hebrew",
  "Korean",
  "Catalan",
  "Dutch",
  "Hungarian",
  "Arabic (Saudi)",
  "Latvian",
  "Slovak",
  "Kiswahili",
  "American Sign Language",
  "British Sign Language",
  "Estonian",
  "Kigiriama",
  "Japanese"
)
