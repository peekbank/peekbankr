# The checks of ds.validate_table() on hand-written tables. Each table check
# also runs on its own, so its cases only need the columns it reads.

library(tibble)

# .msg() wraps a long message over several elements, starting each new one
# with a space, so pasting them back together gives the message again
error_text <- function(result) paste(result$errors, collapse = "")

aoi_timepoints <- tibble(aoi_timepoint_id = 0:2, aoi = "target",
                         administration_id = 0L, t_norm = c(0L, 25L, 50L),
                         trial_id = 0L)

cdi_json <- function(language = "English (American)", rawscore = 300) {
  sprintf('{"instrument_type": "ws", "measure": "prod", "age": 24, "rawscore": %s, "language": "%s"}',
          rawscore, language)
}
subjects <- function(...) {
  tibble(lab_subject_id = paste0("s", seq_along(c(...))), subject_aux_data = c(...))
}
with_cdi <- function(...) sprintf('{"cdi_responses": [%s]}', paste(c(...), collapse = ", "))

test_that("ds.validate_table passes a valid table", {
  expect_equal(ds.validate_table(aoi_timepoints, "aoi_timepoints", FALSE, "."),
               list(errors = NULL, warnings = list()))
})

test_that("ds.validate_table reports timepoints off the 40 Hz grid once", {
  off_grid <- dplyr::mutate(aoi_timepoints, t_norm = c(0L, 30L, 50L))
  expect_equal(ds.validate_table(off_grid, "aoi_timepoints", FALSE, ".")$errors,
               "- Column t_norm in table aoi_timepoints is not sampled at 40HZ.")
})

test_that("ds.validate_table reports a missing t_norm instead of stopping", {
  out <- ds.validate_table(dplyr::select(aoi_timepoints, -"t_norm"), "aoi_timepoints", FALSE, ".")
  expect_match(error_text(out), "Cannot locate required field: t_norm")
})

test_that("check_fields holds the columns against the schema", {
  out <- peekbankr:::check_fields(
    dplyr::mutate(aoi_timepoints[1:2, ], aoi_timepoint_id = c(1L, 3L), trial_id = NULL,
                  extra = 1),
    "aoi_timepoints", TRUE)
  expect_match(error_text(out), "invalid columns found in aoi_timepoints: extra")
  expect_match(error_text(out), "Cannot locate required field: trial_id")
  expect_match(error_text(out), "aoi_timepoint_id should start at 0")
  expect_match(error_text(out), "aoi_timepoint_id is missing ids in its sequence")
})

test_that("check_fields lets nullable fields be left out when asked to", {
  expect_match(error_text(peekbankr:::check_fields(tibble(subject_id = 0L), "subjects", TRUE)),
               "Cannot locate required field: sex")
  expect_null(peekbankr:::check_fields(tibble(subject_id = 0L), "subjects", FALSE)$errors)
})

test_that("check_subjects names the rows with native languages the schema lacks", {
  out <- peekbankr:::check_subjects(tibble(native_language = c("eng", "eng, spa", "english")))
  expect_match(error_text(out), "row\\(s\\) 3 do not belong")
  # a missing column is check_fields()'s to report
  expect_equal(peekbankr:::check_subjects(tibble(subject_id = 0L)), list())
})

test_that("check_subject_aux holds CDI data against cdi_expected", {
  expect_equal(peekbankr:::check_subject_aux(subjects(with_cdi(cdi_json())), TRUE),
               list(errors = NULL, warnings = list()))
  expect_equal(peekbankr:::check_subject_aux(subjects(with_cdi(cdi_json())), FALSE)$errors,
               "No cdi expected, but CDI data found")
  no_cdi <- subjects('{"lang_exposures": [{"language": "eng", "exposure": 50}]}')
  expect_match(error_text(peekbankr:::check_subject_aux(no_cdi, TRUE)), "No CDI data found")
})

test_that("check_subject_aux wants Wordbank's language names", {
  expect_output(out <- peekbankr:::check_subject_aux(subjects(with_cdi(cdi_json("English"))), TRUE),
                "English")
  expect_match(error_text(out), "CDI responses that have a language that does not follow")

  measures <- subjects('{"lang_measures": [{"instrument_type": "x", "language": "Klingon", "rawscore": 3}]}')
  expect_match(error_text(peekbankr:::check_subject_aux(measures, FALSE)),
               "lang_measures entries with a language .* Klingon")
})

test_that("check_subject_aux warns about a subject's duplicate CDI scores", {
  out <- peekbankr:::check_subject_aux(
    subjects(with_cdi(cdi_json(rawscore = 300), cdi_json(rawscore = 310)), with_cdi(cdi_json())),
    TRUE)
  expect_null(out$errors)
  expect_equal(out$warnings, list(cdi_collision = "- Duplicate CDI entries found for subject(s): s1"))
})

test_that("check_trial_types and check_stimuli want rows that differ beyond their id", {
  expect_match(error_text(peekbankr:::check_trial_types(tibble(trial_type_id = 0:1, condition = "a"))),
               "trial types are not unique")
  expect_null(peekbankr:::check_trial_types(tibble(trial_type_id = 0:1, condition = c("a", "b")))$errors)

  dir <- tempfile()
  dir.create(file.path(dir, "processed_data"), recursive = TRUE)
  dir.create(file.path(dir, "raw_data"))
  stimuli <- tibble(stimulus_id = 0:1, stimulus_image_path = NA_character_)
  expect_match(error_text(peekbankr:::check_stimuli(stimuli, file.path(dir, "processed_data"))),
               "stimulus entries are not unique")
})

test_that("check_stimuli wants images that exist, in jpg or png", {
  dir <- tempfile()
  dir.create(file.path(dir, "processed_data"), recursive = TRUE)
  dir.create(file.path(dir, "raw_data"))
  file.create(file.path(dir, "raw_data", c("dog.jpg", "cat.gif")))
  stimuli <- tibble(stimulus_id = 0:2, stimulus_image_path = c("dog.jpg", "cat.gif", "Dog.jpg"))

  out <- peekbankr:::check_stimuli(stimuli, file.path(dir, "processed_data"))
  # case matters even where the file system ignores it
  expect_equal(out$errors, "- stimulus filepaths not found: Dog.jpg")
  expect_match(paste(out$warnings$stimulus_image_filetype, collapse = ""),
               "unsupported format .*: cat.gif$")

  expect_message(peekbankr:::check_stimuli(stimuli, file.path(tempfile(), "processed_data")),
                 "raw_data directory not found")
})

test_that("check_administrations converts lab ages by their unit", {
  # whole years count from the middle of the year
  admins <- tibble(lab_age = c(18, 2, 730.5), lab_age_units = c("months", "years", "days"),
                   age = c(18, 30, 24))
  expect_null(peekbankr:::check_administrations(admins)$errors)
  expect_match(error_text(peekbankr:::check_administrations(dplyr::mutate(admins, age = c(18, 24, 24)))),
               "some ages do not match")
})

test_that("check_trials wants excluded set, and reasons only on excluded trials", {
  # an NA excluded is reported once, with or without a reason
  expect_equal(error_text(peekbankr:::check_trials(tibble(excluded = c(FALSE, NA),
                                                          exclusion_reason = c(NA, "fussy")))),
               "- Column 'excluded' contains NA values. All trials must have excluded set to TRUE or FALSE.")
  expect_match(error_text(peekbankr:::check_trials(tibble(excluded = c(FALSE, TRUE),
                                                          exclusion_reason = "fussy"))),
               "exclusion reasons even though they are marked as included")
  expect_null(peekbankr:::check_trials(tibble(excluded = c(FALSE, TRUE),
                                              exclusion_reason = c("", "fussy")))$errors)
})
