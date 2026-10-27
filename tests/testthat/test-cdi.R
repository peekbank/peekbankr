# The CDI pipeline end to end over a real release. Pinned, so a failure here
# means the code broke or that release changed, not that a different release
# happened to be published most recently. test-cdi-processing.R covers the
# same functions on fixtures, without a network.

test_that("CDI pipeline runs on real subject aux data", {
  skip_network()

  con <- suppressMessages(connect_to_peekbank(db_version = "2026.1"))
  all_subjects <- unpack_aux_data(get_subjects(connection = con))

  cdi_data <- all_subjects %>%
    tidyr::unnest("subject_aux_data") %>%
    dplyr::filter(!is.na(.data$cdi_responses)) %>%
    tidyr::unnest("cdi_responses") %>%
    peekbankr::cleanup_cdi_data() %>%
    peekbankr::append_relative_cdi_scores()

  expect_gt(nrow(cdi_data), 0)
  expect_true(all(c("instrument_type", "rawscore") %in% names(cdi_data)))

  expect_no_error(peekbankr::populate_cdi_percentiles(cdi_data))
})

test_that("unnest_aux and the CDI additions run on real subject aux data", {
  skip_network()

  pb <- suppressMessages(peekbank("2026.1"))
  cdi <- unnest_aux(pb$subjects, "cdi_responses") %>%
    add_cdi_percentiles() %>%
    add_cdi_proportions()

  expect_gt(nrow(cdi), 0)
  expect_true(all(c("subject_id", "dataset_name", "rawscore", "norm_percentile",
                    "proportion") %in% names(cdi)))
  expect_true(any(!is.na(cdi$norm_percentile)))
})
