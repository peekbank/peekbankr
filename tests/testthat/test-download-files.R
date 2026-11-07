# pb_download_files offline: a fake fetch writes `size` bytes to dest

fake_files <- dplyr::tibble(file_id = c("a", "b"),
                            file_name = c("d/a.png", "d/b.png"),
                            size = c(3, 5))

fake_write <- function(dest, size) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(raw(size), dest)
  dest
}

test_that("pb_download_files only fetches what is not on disk at its full size", {
  dir <- tempfile()
  fake_write(file.path(dir, "d/a.png"), 3)
  fake_write(file.path(dir, "d/b.png"), 1)
  fetched <- NULL
  local_mocked_bindings(pb_fetch_batch = function(files) {
    fetched <<- files$file_name
    purrr::walk2(files$dest, files$size, fake_write)
  })

  msgs <- capture_messages(local <- pb_download_files(fake_files, dir))

  expect_equal(fetched, "d/b.png")
  expect_equal(local, file.path(dir, fake_files$file_name))
  expect_match(msgs, "Downloaded 1 files (1 already on disk, 0 failed)",
               fixed = TRUE, all = FALSE)
})

test_that("a failed batch goes file by file, and a rejected file is only NA itself", {
  dir <- tempfile()
  not_found <- structure(class = c("redivis_api_error", "error", "condition"),
                         list(message = "Not found", status_code = 404))
  local_mocked_bindings(
    pb_fetch_batch = function(files) stop("batch down"),
    pb_fetch_file = function(file_id, file_name, size, dest) {
      if (file_id == "a") stop(not_found)
      fake_write(dest, size)
    })

  msgs <- capture_messages(local <- pb_download_files(fake_files, dir))

  expect_equal(local, c(NA, file.path(dir, "d/b.png")))
  expect_match(msgs, "batch down", all = FALSE)
  expect_match(msgs, "Downloaded 1 files (0 already on disk, 1 failed)",
               fixed = TRUE, all = FALSE)
  expect_match(msgs, "Failed to download: d/a.png", all = FALSE)
})
