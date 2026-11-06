# network tests against the peekbank dataset on Redivis; skip_network() is
# defined in helper-network.R

test_that("connect_to_peekbank resolves the current version", {
  skip_network()
  con <- suppressMessages(connect_to_peekbank())
  expect_s3_class(con, "peekbank_connection")
  expect_match(con$release, "^\\d{4}\\.\\d+$")
  expect_match(con$redivis_tag, "^v\\d+\\.\\d+$")
})

test_that("connect_to_peekbank resolves a release name to its version", {
  skip_network()
  con <- suppressMessages(connect_to_peekbank(db_version = "2021.1"))
  expect_equal(con$release, "2021.1")
  expect_match(con$redivis_tag, "^v[0-9]+\\.[0-9]+$")
  # the tag is reported, not accepted: a version is named by its release
  expect_error(
    suppressMessages(connect_to_peekbank(db_version = con$redivis_tag)),
    "not found")
})

test_that("connect_to_peekbank rejects unknown versions", {
  skip_network()
  expect_error(suppressMessages(connect_to_peekbank(db_version = "1999.9")),
               "not found")
})

test_that("deprecated connection arguments warn", {
  skip_network()
  expect_warning(suppressMessages(connect_to_peekbank(host = "localhost")),
                 "deprecated")
})

test_that("get_db_info returns the discovered release map", {
  skip_network()
  info <- get_db_info()
  expect_true(!is.null(info$current))
  expect_true("2021.1" %in% names(info$versions))
})

test_that("get_datasets returns data on the current version", {
  skip_network()
  con <- suppressMessages(connect_to_peekbank())
  datasets <- get_datasets(connection = con)
  expect_gt(nrow(datasets), 0)
  expect_true(all(c("dataset_id", "dataset_name") %in% names(datasets)))
})
