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

# Guard on the workaround in pb_fetch_file(): redivis's own file helpers crash
# on the peekbank file tree, so we build files from their ids instead. This
# fails once that is fixed upstream, which is the signal to drop the workaround.
test_that("redivis's file helpers still crash on the peekbank file tree", {
  skip_network()
  worked <- tryCatch({
    pb_files_dataset()$table("files")$file("pomper_saffran_2016/README.md")
    TRUE
  }, error = function(e) FALSE)

  if (worked) {
    fail(paste0(
      "redivis's $file() no longer crashes, so the workaround is obsolete.\n",
      "In R/redivis.R, replace the body of pb_fetch_file() with:\n",
      "  pb_files_dataset()$table(\"files\")$file(file_name)$download(\n",
      "    path = dest, overwrite = TRUE, progress = FALSE)\n",
      "dropping the utils::getFromNamespace() call and the comment above it, ",
      "then delete this test."))
  }
  succeed()
})
