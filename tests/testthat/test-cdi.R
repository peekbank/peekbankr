# CDI-processing smoke test against the current Redivis release.

test_that("CDI pipeline runs on current-release subject aux data", {
  skip_network()

  con <- suppressMessages(connect_to_peekbank(db_version = "current"))
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
