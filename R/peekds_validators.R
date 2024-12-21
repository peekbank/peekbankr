#' Check if a dataframe/table is compliant to peekbank json before database
#' import
#'
#' @param df_table the dataframe to be saved
#' @param table_type the type of dataframe, for the most updated table types
#'   specified by schema, please use functionds.list_ds_tables()
#' @param is_null_field_required by default is set to TRUE which means that
#'   all the columns in the json file are required; when user specifically
#'   sets this to FALSE, then the fields that are allowed null values are not
#'   required.
#'
#' @return an empty string when the input data frame is compliant with json
#'   specification, such as having all the required columns, primary key field
#'   has unique values, etc. Otherwise, the function returns a list of messages
#'   describing detailed issues that needs to be fixed
#'
#' @examples
#' \dontrun{
#' is_valid <-ds.validate_table(df_table = df_table, table_type = "xy_data", cdi_expected = F, dir_csv = "../processed_data")
#' }
#'
#' @export
ds.validate_table <- function(df_table, table_type, cdi_expected, dir_csv, is_null_field_required = TRUE) {

  msg_error <- c()
  colnames_table <- colnames(df_table)

  fields_json <-ds.get_json_fields(table_type = table_type)
  fieldnames_json <- fields_json$field_name

  unwanted_columns <- setdiff(colnames_table, fieldnames_json)
  if(length(unwanted_columns) > 0){
    msg_error <- c(msg_error, .msg("- invalid columns found in {table_type}: {unwanted_columns}."))
  }

  # start checking field/column one by one
  for (idx in 1:length(fieldnames_json)) {
    fieldname <- fieldnames_json[idx]
    fieldclass <- fields_json$field_class[idx]
    fieldoptions <- fields_json$options[idx, ]

    idx_tb <- match(fieldname, colnames_table)
    is_primary <- isTRUE(fieldoptions$primary_key)
    is_null_allowed <- fieldoptions$null
    is_field_missing <- is.na(idx_tb)

    # when user specifically sets this to FALSE, then the fields that are
    # allowed null values are not required.
    if (!is_null_field_required & is_null_allowed & is_field_missing) {
      #warning("Field {fieldname} is not present, but it is not a required field.")
      next
    }

    # step 0: check if this is a required field
    # if this column is optional, we skip
    if (is_field_missing) {
      msg_new <- .msg("- Cannot locate required field: {fieldname}. Please add
                      the column into the {table_type} processed data file.")
      msg_error <- c(msg_error, msg_new)
      next
    }

    # step 1: check if values in primary_key and unique-option fields are unique
    content_tb <- df_table %>% dplyr::pull(fieldname)

    if (is_primary | isTRUE(fieldoptions$unique)) {
      # first check if primary key is in integer forms, start from zero and are sequential
      if (is_primary) {
        content_tb <- as.integer(content_tb)
        if (min(content_tb) != 0) {
          msg_new <- .msg("- Primary key field {fieldname} should start at 0.")
          msg_error <- c(msg_error, msg_new)
        }
        # check if ids are incremented correctly
        if(length(content_tb) > 1 && any(na.omit(sort(content_tb) - dplyr::lag(sort(content_tb)) != 1))){
          msg_new <- .msg("- Primary key field {fieldname} is missing ids in its sequence. IDs must start at 0 and increment by 1 each")
          msg_error <- c(msg_error, msg_new)
        }

      }
      # make sure primary key and unique fields have unique values
      is_unique <- length(content_tb) == length(unique(content_tb))
      if (!is_unique) {
        msg_new <- .msg("- The values in field {fieldname} are not unique.")
        msg_error <- c(msg_error, msg_new)
      }
      if (sum(is.na(content_tb)) > 0) {
        msg_new <- .msg("- Column {fieldname} cannot contain NA values.")
        msg_error <- c(msg_error, msg_new)
      }
    }

    # step 1.5 check if any aux data is a top level list by mistake
    if(grepl("aux", fieldname) && any(grepl("^\\[", content_tb))){
      msg_new <- .msg("- {fieldname} should be an object at the top level or NA, not list.")
      msg_error <- c(msg_error, msg_new)
    }

    # step 2: check if values are in the required type/format
    if (!is_null_allowed & (fieldclass == "IntegerField" | fieldclass == "ForeignKey")) {
      is_type_valid <- is.integer(content_tb)
      if (!is_type_valid) {
        msg_new <- .msg("- ForeignKey column {fieldname} should contain integers only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else if (!is_null_allowed & fieldclass == "CharField") {
      # numbers are allowed here as well since numbers can be converted into
      # chars
      is_type_valid <- is.character(content_tb) |
        (typeof(content_tb) == "integer")
      if (!is_type_valid) {
        msg_new <- .msg("- Column {fieldname} should contain characters only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else {
      # no required data type
      is_type_valid <- TRUE
    }

    # step 3: if word/label choices are required, check if the values are valid
    if ("choices" %in% colnames(fieldoptions)) {
      choices_json <- unique(unlist(fieldoptions$choices))

      if (is_type_valid & (length(choices_json) > 0)) {
        is_value_valid <- all(unique(content_tb) %in% choices_json)
        if (!is_value_valid) {
          msg_new <- .msg("- Column {fieldname} should contain the following
                          values only: {paste(choices_json, collapse = ', ')}.")
          msg_error <- c(msg_error, msg_new)
        }
      }
    }

    # STEP 4.1:
    # if aoi/xy_timepoints table, then check if resampling was done
    if (table_type == "aoi_timepoints") {
      remainder <- unique(df_table$t_norm %% pkg_globals$SAMPLE_DURATION)
      if (length(remainder) > 1 || remainder != 0) {
        msg_new <- .msg("- Column t_norm in table {table_type} is notsampled
                        at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }
    if (table_type == "xy_timepoints") {
      remainder <- unique(df_table$t_norm %% pkg_globals$SAMPLE_DURATION)
      if (length(remainder) > 1 || remainder != 0) {
        msg_new <- .msg("- Column t_norm in table {table_type} is not sampled
                        at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }

    # STEP 4.2:
    # if subjects table, then check if native_language field was entered
    # correctly
    if (table_type == "subjects" && fieldname == "native_language") {
      language_list <-ds.list_language_choices()

      # go through every native language in the subjects table, check if all the
      # language codes are in the allowed list from json file
      invalid_languages <- df_table %>%
        dplyr::mutate(
          row_number = 1:dplyr::n(),
          valid_language = .data$native_language %>%
            purrr::map_lgl(function(lang) {
              all(stringr::str_trim(stringr::str_split(lang, ",")[[1]]) %in%
                    language_list)
            })
        ) %>%
        dplyr::filter(!.data$valid_language)

      if (nrow(invalid_languages) != 0) {
        msg_new <- .msg("- The subjects' native languages in the following entry
                        row(s) {invalid_languages$row_number} do not belong in
                        the allowed language list in json. Please see function
                       ds.list_language_choices().")
        msg_error <- c(msg_error, msg_new)
      }
    }
  }

  # STEP 4.3:
  # if subjects table, then check if cdi_responses in subject_aux_data
  # has the correct format

  if (table_type == "subjects") {
    # unpack subject aux data from JSON
    sad <- df_table %>%
      dplyr::select(lab_subject_id, subject_aux_data) %>%
      peekbankr:::unpack_aux_data() %>%
      tidyr::unnest(subject_aux_data)

    if(cdi_expected && !("cdi_responses" %in% colnames(sad))){
      msg_error <- c(msg_error, "No CDI data found, check subjects.csv and the column specification")
    }

    if ("cdi_responses" %in% colnames(sad)) {
      if(!cdi_expected){
        msg_error <- c(msg_error, "No cdi expected, but CDI data found")
      }

      sad_cdi <- sad %>%
        # only keep those with cdi data
        dplyr::filter(sapply(.data[["cdi_responses"]], class) == "data.frame")

      # check that each CDI response has the correct columns
      msg_new <- NULL

      purrr::walk(sad_cdi$cdi_responses, \(cdi) {

        if(nrow(cdi) == 0){
          return()
        }

        columns_to_check <- c("instrument_type", "measure", "age", "rawscore", "language")

        missing_cdi <- setdiff(columns_to_check,
                               colnames(cdi))

        if (length(missing_cdi) > 0) {
          msg_new <<- .msg("- Some subject(s) have CDI responses that are missing
                          the following required fields: {missing_cdi}.")
        } else {
          columns_with_na <- columns_to_check[sapply(cdi[columns_to_check], function(x) any(is.na(x)))]

          if (length(columns_with_na) > 0) {
            msg_new <<- .msg("- Some subject(s) have CDI responses with NA values in the following required fields:
                   {paste(columns_with_na, collapse=', ')}.")
          }
        }
      })

      if(!is.null(msg_new)){
        msg_error <- c(msg_error, msg_new)
      }

      # check for correct format of CDI response columns
      cdi <- sad_cdi %>%
        tidyr::unnest(cdi_responses)

      if (any(!(cdi$instrument_type %in% c("wg", "ws", "wsshort", "wgshort")))) {
        msg_new <- .msg("- Some subject(s) have CDI responses that have an
                          incorrect instrument_type.")
        msg_error <- c(msg_error, msg_new)
      }
      if (any(!(cdi$measure %in% c("prod", "comp")))) {
        msg_new <- .msg("- Some subject(s) have CDI responses that have an
                          incorrect measure")
        msg_error <- c(msg_error, msg_new)
      }

      if (any(!is.numeric(cdi$age))) {
        msg_new <- .msg("- Some subject(s) have CDI responses that have a
                          non-numeric age.")
        msg_error <- c(msg_error, msg_new)
      }
      if (any(!is.numeric(cdi$rawscore))) {
        msg_new <- .msg("- Some subject(s) have CDI responses that have a
                          non-numeric rawscore.")
        msg_error <- c(msg_error, msg_new)
      }
      if ("percentile" %in% colnames(cdi)){ # separate conditions as & does not short-circut
          if(any(!is.numeric(cdi$percentile))) {
        msg_new <- .msg("- Some subject(s) have CDI responses that have a
                          non-numeric percentile.")
        msg_error <- c(msg_error, msg_new)
          }
        }
      if (any(class(cdi$language) != "character")) {
        msg_new <- .msg("- Some subject(s) have CDI responses that have a
                          non-character language.")
        msg_error <- c(msg_error, msg_new)
      }

      if (any(!(cdi$language %in% pkg_globals$WORDBANK_ALLOWED_LANGUAGES))) {
        print(setdiff(cdi$language, pkg_globals$WORDBANK_ALLOWED_LANGUAGES))
        msg_new <- .msg("- Some subject(s) have CDI responses that have a language that does not follow the Wordbank specification.")
        msg_error <- c(msg_error, msg_new)
      }
    }
  }

  # STEP 5: trial_types table
  if (table_type == "trial_types"){

    # check if there are any entries that are duplicate and only differ by id
    if((df_table %>% select(-trial_type_id) %>% distinct() %>% nrow()) != (df_table %>% nrow())){
      msg_new <- .msg("- trial types are not unique.")
      msg_error <- c(msg_error, msg_new)
    }
  }

  # STEP 6:
  # if stimuli table, check if there are any entries that are duplicate and only differ by id
  if (table_type == "stimuli"){
    if((df_table %>% select(-stimulus_id) %>% distinct() %>% nrow()) != (df_table %>% nrow())){
      msg_new <- .msg("- stimulus entries are not unique.")
      msg_error <- c(msg_error, msg_new)
    }
  }

  # STEP 6.5:
  # if stimuli table, check if there are any invalid image file paths (dont exist or wrong filetype)
  if (table_type == "stimuli"){

    to_check <- df_table %>% dplyr::filter(!is.na(stimulus_image_path))

    raw_data_dir <- file.path(dir_csv, '..', "raw_data")
    if (!dir.exists(raw_data_dir)) {
      print("Attention: raw_data directory not found at ", raw_data_dir, "the current setup expects the raw_data folder to live next to the processed_data folder. If this is not the case, the image filepath checking will not work properly")
    }
    not_found <- to_check %>% dplyr::filter(
      !file.exists(file.path(raw_data_dir, stimulus_image_path))
    )

    if(nrow(not_found)){
      missing_files <- paste(not_found$stimulus_image_path, collapse = "\n  ")
      msg_new <- .msg(sprintf("- stimulus filepaths not found:\n  %s", missing_files))
      msg_error <- c(msg_error, msg_new)
    }

    wrong_filetype <- to_check %>% dplyr::filter(
      !grepl("\\.(jpe?g|png)$", stimulus_image_path)
    )

    if(nrow(wrong_filetype)){
      wrong_files <- paste(wrong_filetype$stimulus_image_path, collapse = ", ")
      print(sprintf("Warning: some stimulus images have a not supported format (jpg, jpeg, png):\n  %s", wrong_files))
    }
  }


  # STEP 7: check if the subjects age is consistently converted in the administration table
  if (table_type == "administrations") {

    # in the case of years, we need to differentiate when converting:
    # if all years are given in full numbers, the conversion is not *12, but rather *12 + 6
    years_given_full <- !any(df_table %>%
                               dplyr::filter(lab_age_units == "years") %>%
                               dplyr::mutate(year_is_decimal = lab_age-floor(lab_age) != 0) %>%
                               dplyr::pull(year_is_decimal)
                             )

    converted_ages <- df_table %>%
      filter(!is.na(lab_age)) %>%
      dplyr::mutate(converted_age = dplyr::case_when(
        lab_age_units == "months" ~ lab_age,
        lab_age_units == "years" ~ lab_age * 12 + ifelse(years_given_full, 6, 0),
        lab_age_units == "days" ~ lab_age/(365.25/12),
        TRUE ~ NA,
        )) %>%
      dplyr::pull(converted_age)

    if(!isTRUE(all.equal(df_table %>% filter(!is.na(lab_age)) %>% pull(age),converted_ages))){

      msg_new <- .msg("- some ages do not match the conversion according to lab_age_unit and lab_age.")
      msg_error <- c(msg_error, msg_new)
    }
  }

  # STEP 8: trials table
  if (table_type == "trials"){

    # check if there are cases where there is an exclusion reason but excluded is false
    if(any(df_table %>% mutate(incorrectly_included = !excluded & (!is.na(exclusion_reason) & exclusion_reason != "")) %>% pull(incorrectly_included))){
      msg_new <- .msg("- some trials have exclusion reasons even though they are marked as included.")
      msg_error <- c(msg_error, msg_new)
    }
  }

  return(msg_error)
}

#' Check if within aoi_timepoints table, there is no duplication in all the administration_ids
#'   associated with each individual trial_id
#'
#' @param df_table the aoi_timepoints dataframe
#' @param cdi_expected specifies whether cdi_data is to be expected to be present in the imported data;
#'      only relevant for subjects table.
#'      We could consider creating a special table type, so that invalid combinations of table_type and cdi_expected cannot happen, but it does not break anything, so low priority
#'
#'
#' @return an empty string when all the administration_ids are unique within each trial_id;
#'   Otherwise, the error message will be returned.
#'
#' @examples
#' \dontrun{
#' is_valid <-ds.validate_table(df_table = df_table, table_type = "xy_data", cdi_expected = FALSE)
#' }
#'
#' @export
ds.validate_trial_uniqueness_constraint <- function(df_aoi_timepoints) {
  msg_error <- c()

  num_admins_per_trial_id = aggregate(administration_id  ~ trial_id , df_aoi_timepoints, function(x){length(unique(x))})

  if (any(num_admins_per_trial_id$administration_id != 1)){
    msg_error <- .msg('Multiple administrations detected for the same trial ID. Make sure that trials are split out by subject to allow subject-specific trial exclusion')
  }

  return(msg_error)
}

#' check all csv files against database schema for database import
#'
#' @param dir_csv the folder directory containing all the csv files, the path
#'   should end in "processed_data"
#' @param file_ext the default is ".csv"
#' @param cdi_expected specifies whether cdi_data is to be expected to be present in the imported data
#'
#' @return an empty string if all tables passed the validator; otherwise, the
#'   function returns a list of messages describing detailed issues that needs
#'   to be fixed
#'
#' @examples
#' \dontrun{
#' msg_error_all <-ds.validate_for_db_import(dir_csv = "./processed_data")
#' }
#'
#' @export
ds.validate_for_db_import <- function(dir_csv, cdi_expected, file_ext = ".csv", is_null_field_required = TRUE) {

  if(missing(cdi_expected)){
    stop("Need to specifiy cdi_expected boolean argument to validator")
  }

  # check coding method
  coding_file <- file.path(dir_csv, paste0(table_type = "administrations",
                                           file_ext))
  if (file.exists(coding_file)) {
    coding_table <- utils::read.csv(coding_file)
    coding_methods <- unique(coding_table[, "coding_method"])
  } else {
    stop("Cannot find required administrations file.")
  }

  # fetch the table list based on coding method
  table_list <-ds.list_ds_tables(coding_methods)
  # admin table is not required
  # table_list <- table_list[table_list != "admin"];
  msg_error_all <- c()

  #######################################################
  # start checking each table format against json
  dict_tables = list()

  for (table_type in table_list) {
    file_csv <- file.path(dir_csv, paste0(table_type, file_ext))
    if (file.exists(file_csv)) {
      # read in csv file and check if the data is valid
      dict_tables[[table_type]] <- utils::read.csv(file_csv)
      msg_error <-ds.validate_table(dict_tables[[table_type]], table_type, cdi_expected, dir_csv, is_null_field_required)
      if (!is.null(msg_error)) {
        msg_error <- .msg("The processed data file {table_type} failed to pass
                          the validator for database import with these error
                          messsages:\n {paste(msg_error, collapse = '\n')}")
        # cat(crayon::bgMagenta(msg_error), "\n")
        message(msg_error)
        msg_error_all <- c(msg_error_all, msg_error)
      } else {
        print(.msg("The processed data file {table_type} passed the
                   validator!"))
      }
    } else if (ds_is_table_required(table_type, coding_methods)) {
      msg_error_all <- c(msg_error_all, .msg("Cannot find required file: {file_csv}"))
    }
  }

  #######################################################
  # start cross-table validation
  msg_error <-ds.validate_trial_uniqueness_constraint(dict_tables[['aoi_timepoints']])
  message(msg_error)
  msg_error_all <- c(msg_error_all, msg_error)

  # check if
  table_list


  # check if there are any duplicate trial order values within each administration

  if(nrow(dict_tables[['administrations']] %>%
          left_join(dict_tables[['aoi_timepoints']], by = join_by(administration_id)) %>%
          left_join(dict_tables[['trials']], by = join_by(trial_id)) %>%
          distinct(administration_id, trial_id, trial_order) %>%
          group_by(administration_id) %>%
          filter(duplicated(trial_order) | duplicated(trial_order, fromLast = TRUE)) %>%
          ungroup()) != 0){
    msg_error_all <- c(msg_error_all, .msg("Global issue: - trials order values are not unique within administrations"))
  }

  table_pairs <- list(
    c("trials", "aoi_timepoints", "trial_id"),
    c("administrations", "aoi_timepoints", "administration_id"),
    c("administrations", "subjects", "subject_id"),
    c("trials", "trial_types", "trial_type_id"),
    c("stimuli", "trial_types", "stimulus_id")
  )

  if("xy_timepoints" %in% table_list){
    table_pairs <- table_pairs %>%
      append(list(c("aoi_region_sets", "trial_types", "aoi_region_set_id"))) %>%
      append(list(c("administrations", "xy_timepoints", "administration_id")))
  }

  # check if there are any orphaned ids left with no connection to other tables
  errors_orphans <- unlist(lapply(
    table_pairs,
    function(vec) {

      table_1 <- dict_tables[[vec[1]]]
      table_2 <- dict_tables[[vec[2]]]
      join_id <- vec[3]

      if(vec[1] == "stimuli" && vec[2] == "trial_types"){
        table_2 <- table_2 %>% pivot_longer(
          cols = c(distractor_id, target_id),
          names_to = "stimulus_type",
          values_to = "stimulus_id"
        )
      }

      errors <- c()

      one_not_in_two <- table_1 %>%
        dplyr::anti_join(table_2, by = join_id)
      if(nrow(one_not_in_two) != 0){
        print(one_not_in_two)
        errors <- c(errors, .msg("Global issue: - not all {join_id} in {vec[1]} have a match in {vec[2]}"))
      }

      two_not_in_one <- table_2 %>%
        dplyr::anti_join(table_1, by = join_id)
      if(nrow(two_not_in_one) != 0){
        print(two_not_in_one)
        errors <- c(errors, .msg("Global issue: - not all {join_id} in {vec[2]} have a match in {vec[1]}"))
      }

      return(errors)
    }
  ))

  msg_error_all <- c(msg_error_all, errors_orphans)

  return(msg_error_all)
}
