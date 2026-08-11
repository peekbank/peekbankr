# network tests against the peekbank dataset on Redivis; skipped on CRAN and
# unless PEEKBANK_NETWORK_TESTS=true (they need internet + redivis auth)

skip_network <- function() {
  skip_on_cran()
  skip_if_not_installed("redivis")
  skip_if(!identical(Sys.getenv("PEEKBANK_NETWORK_TESTS"), "true"),
          "PEEKBANK_NETWORK_TESTS not set")
}

test_that("connect_to_peekbank resolves the current version", {
  skip_network()
  con <- suppressMessages(connect_to_peekbank())
  expect_s3_class(con, "peekbank_connection")
  expect_match(con$release, "^\\d{4}\\.\\d+$")
  expect_match(con$tag, "^v\\d+\\.\\d+$")
})

test_that("connect_to_peekbank resolves release names and raw tags", {
  skip_network()
  con <- suppressMessages(connect_to_peekbank(db_version = "2021.1"))
  expect_equal(con$release, "2021.1")
  con2 <- suppressMessages(connect_to_peekbank(db_version = con$tag))
  expect_equal(con2$release, "2021.1")
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
