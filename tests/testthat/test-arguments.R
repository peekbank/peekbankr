# offline unit tests for argument validation and the connection handle

test_that("validate_dataset_args rejects mixed-up argument types", {
  expect_error(get_administrations(dataset_id = "pomper_saffran_2016"),
               "dataset_id must be numeric")
  expect_error(get_administrations(dataset_name = 12),
               "dataset_name must be a character string")
})

test_that("resolve_connection rejects non-handle connections", {
  expect_error(get_datasets(connection = "not a connection"),
               "must be a peekbank_connection")
  expect_error(get_datasets(connection = list(host = "localhost")),
               "must be a peekbank_connection")
})

test_that("peekbank_connection prints its release and tag", {
  con <- structure(list(release = "2026.1", tag = "v1.4"),
                   class = "peekbank_connection")
  expect_output(print(con), "release 2026.1")
  expect_output(print(con), "v1.4")
})

test_that("sql_id_filter builds IN clauses and drops NAs", {
  expect_equal(peekbankr:::sql_id_filter("administration_id", c(1, 2, 3)),
               "administration_id IN (1, 2, 3)")
  # NA ids are dropped: in the retired MySQL backend, `IN (..., NULL)`
  # never matched, so dropping preserves behavior
  expect_equal(peekbankr:::sql_id_filter("x", c(1, NA, 3)),
               "x IN (1, 3)")
  # no ids at all -> a condition that matches nothing
  expect_equal(peekbankr:::sql_id_filter("x", c(NA_integer_)), "FALSE")
  expect_equal(peekbankr:::sql_id_filter("x", integer(0)), "FALSE")
  # large ids must not go scientific
  expect_equal(peekbankr:::sql_id_filter("x", 123456789),
               "x IN (123456789)")
})
