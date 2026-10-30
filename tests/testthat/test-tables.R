library(tibble)

test_that("the 0/1 flags become logical", {
  trials <- tibble(trial_id = 1:2, excluded = c(1L, 0L))
  expect_identical(peekbankr:::as_logical_flags("trials", trials)$excluded,
                   c(TRUE, FALSE))
  types <- tibble(trial_type_id = 1:2, vanilla_trial = c(0L, 1L))
  expect_identical(
    peekbankr:::as_logical_flags("trial_types", types)$vanilla_trial,
    c(FALSE, TRUE))
})

test_that("a release without the flag columns is left alone", {
  # trials and trial_types up to 2022.1 carry neither column
  old_types <- tibble(trial_type_id = 1:2, target_id = 3:4)
  expect_identical(peekbankr:::as_logical_flags("trial_types", old_types),
                   old_types)
  old_trials <- tibble(trial_id = 1:2, trial_order = 1:2)
  expect_identical(peekbankr:::as_logical_flags("trials", old_trials),
                   old_trials)
})

test_that("a table with no flags of its own is left alone", {
  subjects <- tibble(subject_id = 1:2, sex = c("male", "female"))
  expect_identical(peekbankr:::as_logical_flags("subjects", subjects),
                   subjects)
})

test_that("the legacy getters keep the 0/1 flags", {
  con <- structure(list(release = "2026.1", redivis_tag = "v1.4"),
                   class = "peekbank_connection")
  tables <- list(
    datasets = tibble(dataset_id = 1, dataset_name = "a"),
    trial_types = tibble(trial_type_id = 1:2, dataset_id = 1,
                         vanilla_trial = c(0L, 1L)),
    trials = tibble(trial_id = 1:2, trial_type_id = 1:2, excluded = c(1L, 0L)))
  local_mocked_bindings(pb_table = function(connection, name) tables[[name]])
  expect_identical(get_trials(connection = con)$excluded, c(1L, 0L))
  expect_identical(get_trial_types(connection = con)$vanilla_trial, c(0L, 1L))
})
