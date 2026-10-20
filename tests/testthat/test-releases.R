# Peekbank's schema has changed between releases -- excluded and
# vanilla_trial arrived in 2025.1, for instance -- and a test that pins one
# release cannot see that. This reads every release once.

test_that("every release's tables load and keep their keys", {
  skip_network()
  keys <- c(datasets = "dataset_id", subjects = "subject_id",
            administrations = "administration_id", trials = "trial_id",
            trial_types = "trial_type_id", stimuli = "stimulus_id",
            aoi_region_sets = "aoi_region_set_id")

  for (release in peekbank_releases()$release) {
    pb <- suppressMessages(peekbank(release))
    for (nm in names(keys)) {
      where <- paste(release, nm)
      table <- pb[[nm]]
      expect_s3_class(table, "data.frame")
      expect_true(keys[[nm]] %in% names(table), info = where)
      # widening a table for a predicate relies on these being unique
      expect_false(anyDuplicated(table[[keys[[nm]]]]) > 0, info = where)
    }
  }
})

test_that("every release can be narrowed by a predicate", {
  skip_network()
  for (release in peekbank_releases()$release) {
    pb <- suppressMessages(peekbank(release))
    one <- pb$datasets$dataset_name[[1]]
    # reaches datasets from administrations, and keeps the table's columns
    narrowed <- pb$subset$administrations(dataset_name == !!one)
    expect_identical(names(narrowed), names(pb$administrations), info = release)
    expect_gt(nrow(narrowed), 0)
    # and the timepoint query builds against this release's schema
    expect_match(captured_sql(pb$subset$aoi_timepoints(dataset_name == !!one)),
                 "SELECT", info = release)
  }
})
