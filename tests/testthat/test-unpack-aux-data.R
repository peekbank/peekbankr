# offline unit tests for unpack_aux_data on synthetic aux JSON

library(tibble)
library(dplyr)

test_that("unpack_aux_data expands scalar and nested aux fields", {
  df <- tibble(
    subject_id = 1:3,
    subject_aux_data = c(
      '{"native_language_non_iso": "eng", "cdi_responses": [{"instrument_type": "ws", "rawscore": 10}, {"instrument_type": "wg", "rawscore": 5}]}',
      NA_character_,
      '{"native_language_non_iso": "spa"}'
    )
  )
  out <- unpack_aux_data(df)
  expect_true("subject_aux_data" %in% names(out))
  expect_equal(nrow(out), 3)

  unnested <- tidyr::unnest(out, "subject_aux_data")
  expect_true("native_language_non_iso" %in% names(unnested))
  expect_true(any(unnested$native_language_non_iso == "eng", na.rm = TRUE))
  expect_true(any(unnested$native_language_non_iso == "spa", na.rm = TRUE))

  # nested cdi_responses stay available as a list column
  expect_true("cdi_responses" %in% names(unnested))
  cdi <- unnested %>%
    filter(subject_id == 1) %>%
    tidyr::unnest("cdi_responses")
  expect_equal(sort(cdi$rawscore), c(5, 10))
})

test_that("unpack_aux_data passes through frames without aux columns", {
  df <- tibble(subject_id = 1:2, sex = c("male", "female"))
  expect_identical(unpack_aux_data(df), df)
})

test_that("unpack_aux_data passes through all-NA aux columns", {
  df <- tibble(subject_id = 1:2, subject_aux_data = c(NA_character_, NA))
  expect_identical(unpack_aux_data(df), df)
})
