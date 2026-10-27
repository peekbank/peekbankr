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

# add_cdi_percentiles and add_cdi_proportions; expected values are read off
# inst/extdata/cdi_norms.csv and cdi_n_items.csv

english <- function(...) {
  tibble(language = "English (American)", ...)
}

test_that("add_cdi_percentiles gives the highest percentile a score reaches", {
  # WG comprehension at 8 months: 5th percentile 1 word, 10th 2, 15th 4
  out <- add_cdi_percentiles(english(instrument_type = "wg", measure = "comp",
                                     age = 8.2, rawscore = c(0, 2, 3)))
  expect_equal(out$norm_age, c(8, 8, 8))
  expect_equal(out$norm_percentile, c(1, 10, 10))
})

test_that("add_cdi_percentiles compares against the same sex when it can", {
  # WS production at 24 months: 50th percentile 279 words overall, 295 for
  # girls, so 280 words is the 45th percentile among girls
  out <- add_cdi_percentiles(english(instrument_type = "ws", measure = "prod",
                                     age = 24, rawscore = 280,
                                     sex = c("female", "unspecified")))
  expect_equal(out$norm_percentile, c(50, 50))
  expect_equal(out$norm_percentile_sex, c(45, NA))
})

test_that("add_cdi_percentiles covers the English short forms", {
  # WS short (Level IIA) at 24 months: 50th percentile 62 words, 55th 66
  out <- add_cdi_percentiles(english(instrument_type = "wsshort",
                                     measure = "prod", age = 24,
                                     rawscore = 63))
  expect_equal(out$norm_percentile, 50)
})

test_that("add_cdi_percentiles leaves scores without norms NA and says so", {
  cdi <- tibble(
    language = c("English (American)", "Spanish (Mexican)"),
    instrument_type = c("wg", "ws"),
    measure = c("comp", "prod"),
    age = c(30, 24),
    rawscore = c(100, 300)
  )
  expect_message(out <- add_cdi_percentiles(cdi),
                 "1 on forms without norms, 1 at ages outside the norms")
  expect_true(all(is.na(out$norm_age)))
  expect_true(all(is.na(out$norm_percentile)))
  # no sex column, so no sex-specific percentile either
  expect_true(all(is.na(out$norm_percentile_sex)))
})

test_that("add_cdi_percentiles names the columns it needs", {
  expect_error(add_cdi_percentiles(english(instrument_type = "wg")),
               "Missing required columns: measure, age, rawscore")
})

test_that("add_cdi_proportions divides by the words on the form", {
  out <- add_cdi_proportions(english(instrument_type = c("ws", "wgshort"),
                                     rawscore = c(340, 89)))
  expect_equal(out$n_items, c(680L, 89L))
  expect_equal(out$proportion, c(0.5, 1))
})

test_that("add_cdi_proportions leaves unknown forms NA and names them", {
  expect_message(
    out <- add_cdi_proportions(english(instrument_type = "not_a_form",
                                       rawscore = 1)),
    "English \\(American\\) not_a_form")
  expect_true(is.na(out$proportion))
})
