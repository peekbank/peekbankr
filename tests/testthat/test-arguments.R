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

test_that("peekbank_connection prints its release and redivis_tag", {
  con <- structure(list(release = "2026.1", redivis_tag = "v1.4"),
                   class = "peekbank_connection")
  expect_output(print(con), "release 2026.1")
  expect_output(print(con), "v1.4")
})

test_that("sql_in_clause builds IN clauses and drops NAs", {
  expect_equal(peekbankr:::sql_in_clause("administration_id", c(1, 2, 3)),
               "administration_id IN (1, 2, 3)")
  # NA ids are dropped, so they match nothing
  expect_equal(peekbankr:::sql_in_clause("x", c(1, NA, 3)),
               "x IN (1, 3)")
  # no ids at all -> a condition that matches nothing
  expect_equal(peekbankr:::sql_in_clause("x", c(NA_integer_)), "FALSE")
  expect_equal(peekbankr:::sql_in_clause("x", integer(0)), "FALSE")
  # large ids must not go scientific
  expect_equal(peekbankr:::sql_in_clause("x", 123456789),
               "x IN (123456789)")
})

test_that("pb_query serves repeat queries from the session cache", {
  env <- peekbankr:::.peekbankr_env
  con <- structure(list(release = "2026.1", redivis_tag = "v1.4"),
                   class = "peekbank_connection")
  sql <- "SELECT sentinel FROM nowhere"
  key <- paste(con$redivis_tag, sql)
  on.exit(rm(list = key, envir = env), add = TRUE)

  env[[key]] <- data.frame(sentinel = 42)
  expect_equal(peekbankr:::pb_query(con, sql)$sentinel, 42)

  # the key is scoped to both the version and the query text
  other_version <- structure(list(release = "2025.1", redivis_tag = "v1.2"),
                             class = "peekbank_connection")
  expect_null(env[[paste(other_version$redivis_tag, sql)]])
  expect_null(env[[paste(con$redivis_tag, "SELECT something else")]])
})
