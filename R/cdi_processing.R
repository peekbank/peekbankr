#' Checks cdi data for inconsistencies, warns about them, and fixes them
#'
#' @param cdi_data a subjects table with unnested cdi data, needs columns "subject_id", "language", "instrument_type", "age", "sex", "measure", "rawscore"
#'
#' @return a cleaned up version of the cdi data
#' @export
#'
#' @examples
#' \dontrun{
#' clean_cdi_data <- all_subjects %>%
#'   unnest(subject_aux_data) %>%
#'   filter(!is.na(cdi_responses)) %>%
#'   unnest(cdi_responses) %>%
#'   peekbankr::cleanup_cdi_data()
#' }
cleanup_cdi_data <- function(cdi_data) {
  # TODO: what else do we need to check here? Ask the others about this

  required_columns <- c(
    "subject_id",
    "language",
    "instrument_type",
    "age",
    "sex",
    "measure",
    "rawscore"
  )

  missing_cols <- setdiff(required_columns, colnames(cdi_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check if there are multiple scores for the same cdi administration within a single participant
  duplicate_removed_rows <- cdi_data %>%
    dplyr::group_by(subject_id, instrument_type, measure, age, language) %>%
    dplyr::filter(rawscore != max(rawscore, na.rm = TRUE))

  if (nrow(duplicate_removed_rows) > 0) {
    print("Warning: there are some duplicate cdi values in your data. These were removed, but you should check the input data.")
    print(duplicate_removed_rows)

    # fix the duplicates for analysis until the input data is fixed
    cdi_data <- cdi_data %>%
      dplyr::group_by(subject_id, instrument_type, measure, age, language) %>%
      dplyr::filter(rawscore == max(rawscore, na.rm = TRUE))
  }

  return(cdi_data)
}

#' Populate the provided cdi data with percentile values for that specific age, instrument_type, measure and language. Loosely based on the work from this repo https://github.com/kachergis/cdi-percentiles/tree/main by George Kachergis and Jess Mankewitz with advice from Virginia Marchman.
#'
#' @param subjects_table a subjects table with unnested cdi data, needs columns "subject_id", "language", "instrument_type", "age", "sex", "measure", "rawscore"
#'
#' @return the input table with added columns containing the reference age used, the reference year used, and both gender specific and general percentile values for the cdi score
#' @export
#'
#' @examples
#' \dontrun{
#' full_cdi_data <- all_subjects %>%
#'   unnest(subject_aux_data) %>%
#'   filter(!is.na(cdi_responses)) %>%
#'   unnest(cdi_responses) %>%
#'   peekbankr::cleanup_cdi_data() %>%
#'   peekbankr::populate_cdi_percentiles()
#' }
populate_cdi_percentiles <- function(subjects_table) {
  required_columns <- c(
    "subject_id",
    "language",
    "instrument_type",
    "age",
    "sex",
    "measure",
    "rawscore"
  )

  output_columns <- c(
    "reference_age",
    "reference_year",
    "percentile_all",
    "percentile_sex",
    "norm_score_all",
    "norm_score_sex"
  )

  missing_cols <- setdiff(required_columns, colnames(subjects_table))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  existing_output_cols <- intersect(output_columns, colnames(subjects_table))
  if (length(existing_output_cols) > 0) {
    stop("Output columns already exist: ", paste(existing_output_cols, collapse = ", "))
  }

  norms_tables <- readRDS(system.file("extdata", "cdi_benchmarks_2022", "norms_tables.rds", package = "peekbankr"))

  cdi_norms_long <- norms_tables %>%
    purrr::imap(\(table, name){
      table %>%
        dplyr::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-age), names_to = "head", values_to = "score") %>%
        dplyr::rename(norm_percentile = age, reference_age = head) %>%
        dplyr::mutate(name = gsub(".csv", "", name, fixed = T)) %>%
        tidyr::separate(name,
          into = c("language", "instrument_type", "measure", "norm_sex"),
          sep = "_"
        ) %>%
        # Add zero rows in one step
        dplyr::bind_rows(., dplyr::distinct(., reference_age, language, instrument_type, measure, norm_sex) %>%
          dplyr::mutate(norm_percentile = 1, score = 0))
    }) %>%
    dplyr::bind_rows() %>%
    # TODO: create a wordbank/iso lookup for all languages
    dplyr::mutate(
      language = ifelse(language == "eng", "English (American)", NA),
      reference_age = as.numeric(reference_age)
    )

  # find reference age for each participants entry
  subject_table_with_ref_age <- subjects_table %>%
    dplyr::inner_join(
      cdi_norms_long %>% dplyr::distinct(reference_age, instrument_type, measure, language),
      by = c(
        "instrument_type",
        "measure", "language"
      ),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(age_diff = abs(age - reference_age)) %>%
    dplyr::group_by(dplyr::across(!c(age_diff, reference_age))) %>%
    dplyr::slice_min(abs(age_diff), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-age_diff)

  subject_table_with_cdi_percentiles <- subject_table_with_ref_age %>%
    dplyr::inner_join(
      cdi_norms_long,
      by = c("instrument_type", "measure", "reference_age", "language"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(score <= rawscore) %>%
    dplyr::group_by(dplyr::across(!c(score, norm_percentile))) %>%
    dplyr::slice_max(score, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "norm_sex", values_from = c("norm_percentile", "score")) %>%
    dplyr::mutate(
      percentile_all = norm_percentile_both,
      percentile_sex = dplyr::case_when(
        sex == "male" ~ norm_percentile_m,
        sex == "female" ~ norm_percentile_f,
        T ~ NA
      ),
      norm_score_all = score_both,
      norm_score_sex = dplyr::case_when(
        sex == "male" ~ score_m,
        sex == "female" ~ score_f,
        T ~ NA
      )
    ) %>%
    dplyr::select(subject_id, instrument_type, measure, age, language, reference_age, percentile_all, percentile_sex, norm_score_all, norm_score_sex) %>%
    dplyr::mutate(reference_year = "2022")

  return(subjects_table %>% dplyr::left_join(
    subject_table_with_cdi_percentiles,
    by = c("subject_id", "instrument_type", "measure", "age", "language")
  ))
}


#' Adds a relative cdi score indicating the percentage of total achievable points the subject got on each given measure
#'
#' @param subjects_table a subjects table with unnested cdi data, needs columns "subject_id", "language", "instrument_type", "measure", "rawscore"
#'
#' @return the input table with an added "cdi_relative" column that contains the percentage of total points gained in the given administrations
#' @export
#'
#' @examples
#' \dontrun{
#' cdi_data <- all_subjects %>%
#'   unnest(subject_aux_data) %>%
#'   filter(!is.na(cdi_responses)) %>%
#'   unnest(cdi_responses) %>%
#'   append_relative_cdi_scores()
#' }
append_relative_cdi_scores <- function(subjects_table) {
  required_columns <- c(
    "subject_id",
    "instrument_type",
    "language",
    "measure",
    "rawscore"
  )

  output_columns <- c(
    "cdi_relative"
  )

  missing_cols <- setdiff(required_columns, colnames(subjects_table))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  existing_output_cols <- intersect(output_columns, colnames(subjects_table))
  if (length(existing_output_cols) > 0) {
    stop("Output columns already exist: ", paste(existing_output_cols, collapse = ", "))
  }

  # TODO: find instrument_length values for all languages
  subjects_table %>%
    dplyr::mutate(
      instrument_length = dplyr::case_when(instrument_type == "ws" ~ 680,
        instrument_type == "wg" & language == "English (American)" ~ 396,
        instrument_type == "wsshort" ~ 100,
        instrument_type == "wg" & language == "Spanish (Mexican)" ~ 428, # TODO: double-check Spanish WG length..
        .default = NA
      ),
      CDI_percent = rawscore / instrument_length
    ) %>%
    dplyr::select(-instrument_length)
}
