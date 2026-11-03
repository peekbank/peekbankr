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

# Guard on the workaround in pb_fetch_file(): redivis's own file helpers crash
# on the peekbank file tree, so we build files from their ids instead. This
# fails once that is fixed upstream, which is the signal to drop the workaround.
test_that("redivis's file helpers still crash on the peekbank file tree", {
  skip_network()
  # the crash recurses on ever-longer regexes and warns at every step
  err <- tryCatch(suppressWarnings(
    pb_files_dataset()$table("newman_vocoded_2012")$file(
      "Study 1 (24 and _ channels)/10ES_order2_averages edited.xls")),
    error = identity)

  if (!inherits(err, "error")) {
    fail(paste0(
      "redivis's $file() no longer crashes, so the workaround is obsolete.\n",
      "In R/redivis.R, fetch files with\n",
      "  pb_files_dataset()$table(folder)$file(file_name)$download(\n",
      "    path = dest, overwrite = TRUE, progress = FALSE)\n",
      "instead of pb_fetch_file(), then delete this test."))
  } else {
    # the crash is an error from R's regex engine; an API error (e.g. a
    # renamed folder or file) means the test no longer reaches it
    expect_false(inherits(err, "redivis_error"), info = conditionMessage(err))
  }
})
