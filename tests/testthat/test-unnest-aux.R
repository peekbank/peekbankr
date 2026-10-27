# offline unit tests for unnest_aux on synthetic aux JSON

library(tibble)

subjects <- tibble(
  subject_id = 1:4,
  sex = c("female", "male", "female", "male"),
  subject_aux_data = c(
    '{"native_language_non_iso": "Greenlandic", "cdi_responses": [{"instrument_type": "ws", "rawscore": 10}, {"instrument_type": "wg", "rawscore": 5}]}',
    NA_character_,
    '{"cdi_responses": [{"instrument_type": "ws", "rawscore": 7}]}',
    '{"lang_exposures": [{"language": "Danish", "exposure": 100}]}'
  )
)

test_that("a key of records gives one row per record, keeping the table's columns", {
  out <- unnest_aux(subjects, "cdi_responses")
  expect_equal(out$subject_id, c(1L, 1L, 3L))
  expect_equal(out$sex, c("female", "female", "female"))
  expect_equal(out$rawscore, c(10, 5, 7))
  expect_false("subject_aux_data" %in% names(out))
})

test_that("a key of one value becomes a column named after it", {
  out <- unnest_aux(subjects, "native_language_non_iso")
  expect_equal(out$subject_id, 1L)
  expect_equal(out$native_language_non_iso, "Greenlandic")
})

test_that("a key no row has gives no rows", {
  out <- unnest_aux(subjects, "not_a_key")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("subject_id", "sex"))
})

test_that("an aux field repeating a table column is dropped with a warning", {
  df <- tibble(
    lab_subject_id = "a",
    subject_aux_data = '{"lang_exposures": [{"language": "Danish", "exposure": 100, "lab_subject_id": "b"}]}'
  )
  expect_warning(out <- unnest_aux(df, "lang_exposures"), "lab_subject_id")
  expect_equal(out$lab_subject_id, "a")
})

test_that("a table without an aux column is refused", {
  expect_error(unnest_aux(tibble(subject_id = 1), "cdi_responses"),
               "exactly one \\*_aux_data column")
})
