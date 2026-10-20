# The CDI helpers on hand-written rows, so they are covered without a release
# to read. test-cdi.R runs the same pipeline over real data.

library(tibble)

cdi_rows <- function(...) {
  tibble(
    subject_id = 1:3,
    language = c("English (American)", "English (American)",
                 "Spanish (Mexican)"),
    instrument_type = c("ws", "wg", "wsshort"),
    measure = c("prod", "comp", "prod"),
    age = c(24, 12, 20),
    sex = c("female", "male", "female"),
    rawscore = c(300, 100, 50),
    ...
  )
}

test_that("cleanup_cdi_data passes clean rows through untouched", {
  cdi <- cdi_rows()
  expect_silent(out <- cleanup_cdi_data(cdi))
  expect_equal(out, cdi)
})

test_that("cleanup_cdi_data keeps the highest of duplicate scores", {
  cdi <- dplyr::bind_rows(cdi_rows(),
                          dplyr::mutate(cdi_rows()[1, ], rawscore = 250))
  expect_warning(out <- suppressMessages(print(cleanup_cdi_data(cdi))),
                 "duplicate cdi values")
  expect_equal(nrow(out), 3)
  expect_equal(out$rawscore[out$subject_id == 1], 300)
})

test_that("cleanup_cdi_data names the columns it needs", {
  expect_error(cleanup_cdi_data(dplyr::select(cdi_rows(), -"rawscore")),
               "Missing required columns: rawscore")
})

test_that("append_relative_cdi_scores scores against the instrument length", {
  out <- append_relative_cdi_scores(cdi_rows())
  expect_equal(out$cdi_relative, c(300 / 680, 100 / 396, 50 / 100))
  # and does not leave its working column behind
  expect_false("instrument_length" %in% names(out))
})

test_that("append_relative_cdi_scores has no length for an unknown instrument", {
  out <- append_relative_cdi_scores(
    dplyr::mutate(cdi_rows(), instrument_type = "not_an_instrument"))
  expect_true(all(is.na(out$cdi_relative)))
})

test_that("append_relative_cdi_scores refuses to overwrite its output", {
  expect_error(append_relative_cdi_scores(cdi_rows(cdi_relative = 0)),
               "Output columns already exist: cdi_relative")
  expect_error(
    append_relative_cdi_scores(dplyr::select(cdi_rows(), -"measure")),
    "Missing required columns: measure")
})

test_that("populate_cdi_percentiles scores against the 2022 norms", {
  out <- populate_cdi_percentiles(cdi_rows())
  expect_true(all(c("reference_age", "reference_year", "percentile_all",
                    "percentile_sex", "norm_score_all", "norm_score_sex")
                  %in% names(out)))
  expect_equal(nrow(out), 3)
  expect_equal(out$reference_year[1:2], c("2022", "2022"))

  # the norms are read at the nearest age they tabulate
  expect_equal(out$reference_age[1:2], c(24, 12))
  expect_true(all(out$percentile_all[1:2] >= 5 & out$percentile_all[1:2] <= 99))
  # and the norm score the percentile was read at cannot exceed the raw score
  expect_true(all(out$norm_score_all[1:2] <= out$rawscore[1:2]))

  # no English norms for the Spanish instrument
  expect_true(is.na(out$percentile_all[3]))
})

test_that("populate_cdi_percentiles reads the sex-specific norms", {
  out <- populate_cdi_percentiles(cdi_rows())
  flipped <- populate_cdi_percentiles(
    dplyr::mutate(cdi_rows(), sex = c("male", "female", "female")))
  # percentile_all does not depend on sex, percentile_sex does
  expect_equal(out$percentile_all, flipped$percentile_all)
  expect_false(isTRUE(all.equal(out$percentile_sex[1:2],
                                flipped$percentile_sex[1:2])))
})

test_that("populate_cdi_percentiles guards its columns", {
  expect_error(populate_cdi_percentiles(cdi_rows(percentile_all = 0)),
               "Output columns already exist: percentile_all")
  expect_error(populate_cdi_percentiles(dplyr::select(cdi_rows(), -"age")),
               "Missing required columns: age")
})
