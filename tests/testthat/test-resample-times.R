# unit tests for ds.resample_times (ported from the former loose script
# tests/resample_times.R, with the same cases)

library(tibble)

make_trial <- function(t_norm, aoi) {
  tibble(t_norm = t_norm, aoi = aoi,
         administration_id = 1, trial_id = 1, point_of_disambiguation = 0)
}

test_that("resample_times handles regularly spaced input", {
  df_trial <- make_trial(c(33, 66, 99, 132, 165),
                         c("target", "target", "missing", "distractor",
                           "distractor"))
  resampled <- ds.resample_times(df_trial, table_type = "aoi_timepoints")
  expect_gt(nrow(resampled), 0)
  expect_true(all(resampled$t_norm %% 25 == 0))

  df_trial2 <- make_trial(c(33, 66, 99, 132, 165),
                          c("target", "target", "distractor", "missing",
                            "distractor"))
  expect_silent(ds.resample_times(df_trial2, table_type = "aoi_timepoints"))
})

test_that("resampled points inside a large gap are missing", {
  df_trial <- make_trial(c(33, 66, 99, 1032, 1065),
                         c("target", "target", "distractor", "missing",
                           "distractor"))
  resampled <- ds.resample_times(df_trial, table_type = "aoi_timepoints")
  gap_points <- resampled[resampled$t_norm > 99 & resampled$t_norm < 1032, ]
  expect_gt(nrow(gap_points), 0)
  expect_true(all(gap_points$aoi == "missing"))
})

test_that("duplicate timepoints error", {
  df_trial <- make_trial(c(1, 33, 33, 99),
                         c("missing", "target", "distractor", "distractor"))
  expect_error(ds.resample_times(df_trial, table_type = "aoi_timepoints"),
               "monotonically increasing")
})

test_that("non-ascending timepoints error", {
  df_trial <- make_trial(c(33, 66, 99, 50, 165),
                         c("target", "target", "missing", "distractor",
                           "distractor"))
  expect_error(ds.resample_times(df_trial, table_type = "aoi_timepoints"),
               "monotonically increasing")
})

test_that("integer t_norm columns do not cause type errors", {
  df_trial <- make_trial(as.integer(c(33, 66, 99, 132, 165)),
                         c("target", "target", "missing", "distractor",
                           "distractor"))
  expect_no_error(ds.resample_times(df_trial, table_type = "aoi_timepoints"))
})

test_that("timepoints already on the resampling grid are handled", {
  df_trial <- make_trial(c(33, 66, 99, 100, 132, 165),
                         c("target", "target", "missing", "target",
                           "distractor", "distractor"))
  expect_no_error(ds.resample_times(df_trial, table_type = "aoi_timepoints"))
})
