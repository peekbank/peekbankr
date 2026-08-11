# CDI-processing smoke test (ported from the former tests/cdi_percentiles.R,
# which ran against the retired grant_2024_dev MySQL schema; now runs on the
# current Redivis release). Network-gated like test-network.R.

test_that("CDI pipeline runs on current-release subject aux data", {
  skip_on_cran()
  skip_if_not_installed("redivis")
  skip_if(!identical(Sys.getenv("PEEKBANK_NETWORK_TESTS"), "true"),
          "PEEKBANK_NETWORK_TESTS not set")

  library(dplyr)
  library(tidyr)

  con <- suppressMessages(connect_to_peekbank(db_version = "current"))
  all_subjects <- unpack_aux_data(get_subjects(connection = con))

  cdi_data <- all_subjects %>%
    unnest("subject_aux_data") %>%
    filter(!is.na(.data$cdi_responses)) %>%
    unnest("cdi_responses") %>%
    peekbankr::cleanup_cdi_data() %>%
    peekbankr::append_relative_cdi_scores()

  expect_gt(nrow(cdi_data), 0)
  expect_true(all(c("instrument_type", "rawscore") %in% names(cdi_data)))

  expect_no_error(peekbankr::populate_cdi_percentiles(cdi_data))
})
