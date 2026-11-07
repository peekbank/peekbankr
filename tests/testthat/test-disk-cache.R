# The disk cache of cache_on_disk clients. All but the last test run offline
# against a fake Redivis on version "vtest".

fake_redivis <- function(env = parent.frame()) {
  downloads <- new.env()
  downloads$n <- 0
  rows <- function() {
    downloads$n <- downloads$n + 1
    dplyr::tibble(dataset_id = 1L, dataset_name = "a")
  }
  testthat::local_mocked_bindings(
    resolve_release = function(version) {
      list(release = "2026.1", redivis_tag = "vtest")
    },
    pb_dataset = function(redivis_tag = NULL) {
      list(table = function(name) list(to_tibble = rows),
           query = function(sql) list(to_tibble = rows))
    },
    .package = "peekbankr", .env = env)
  downloads
}

# what a new R session starts from: nothing in memory, the files still there
forget_session <- function(redivis_tag = "vtest") {
  env <- peekbankr:::.peekbankr_env
  keys <- ls(env)
  rm(list = keys[startsWith(keys, paste0(redivis_tag, " "))], envir = env)
}

in_fresh_dir <- function(code) {
  dir <- tempfile("project")
  dir.create(dir)
  old <- setwd(dir)
  on.exit({
    setwd(old)
    unlink(dir, recursive = TRUE)
    forget_session()
  })
  force(code)
}

test_that("a later session reads tables and queries from disk", {
  downloads <- fake_redivis()
  in_fresh_dir({
    pb <- peekbank("2026.1", cache_on_disk = TRUE)
    table <- pb$datasets
    query <- pb$query("SELECT 1")
    expect_equal(downloads$n, 2)

    forget_session()
    pb <- peekbank("2026.1", cache_on_disk = TRUE)
    expect_identical(pb$datasets, table)
    expect_identical(pb$query("SELECT 1"), query)
    expect_equal(downloads$n, 2)
  })
})

test_that("an entry read from disk is restored to the session cache", {
  fake_redivis()
  in_fresh_dir({
    pb <- peekbank("2026.1", cache_on_disk = TRUE)
    table <- pb$datasets

    forget_session()
    invisible(pb$datasets)
    expect_identical(peekbankr:::.peekbankr_env[["vtest datasets"]], table)
  })
})

test_that("a damaged file is replaced by a fresh download", {
  downloads <- fake_redivis()
  in_fresh_dir({
    pb <- peekbank("2026.1", cache_on_disk = TRUE)
    table <- pb$datasets
    path <- list.files(file.path(".pbcache", "vtest"), full.names = TRUE)
    bytes <- readBin(path, "raw", file.size(path))
    damaged <- list(raw(0), bytes[seq_len(length(bytes) %/% 2)],
                    charToRaw("not an rds file"))

    for (content in damaged) {
      writeBin(content, path)
      forget_session()
      expect_message(read <- pb$datasets, "could not be read")
      expect_identical(read, table)
    }
    expect_equal(downloads$n, 4)

    # the fresh copy is what the next session reads
    forget_session()
    expect_identical(pb$datasets, table)
    expect_equal(downloads$n, 4)
  })
})

test_that("without cache_on_disk nothing reaches the disk", {
  downloads <- fake_redivis()
  in_fresh_dir({
    pb <- peekbank("2026.1")
    invisible(pb$datasets)
    expect_false(dir.exists(".pbcache"))

    forget_session()
    invisible(pb$datasets)
    expect_equal(downloads$n, 2)
  })
})

test_that("an entry already in memory is saved by a cache_on_disk client", {
  downloads <- fake_redivis()
  in_fresh_dir({
    invisible(peekbank("2026.1")$datasets)
    invisible(peekbank("2026.1", cache_on_disk = TRUE)$datasets)
    expect_equal(downloads$n, 1)

    forget_session()
    invisible(peekbank("2026.1", cache_on_disk = TRUE)$datasets)
    expect_equal(downloads$n, 1)
  })
})

test_that("a failed read is not saved", {
  in_fresh_dir({
    con <- structure(list(release = "2026.1", redivis_tag = "vtest",
                          cache_dir = file.path(getwd(), ".pbcache")),
                     class = "peekbank_connection")
    expect_null(peekbankr:::cached_read(con, "vtest failed", function() NULL))
    expect_length(list.files(".pbcache", recursive = TRUE), 0)
  })
})

test_that("a failed save warns and still returns the data", {
  in_fresh_dir({
    file.create("not_a_folder")
    con <- structure(list(release = "2026.1", redivis_tag = "vtest",
                          cache_dir = file.path(getwd(), "not_a_folder")),
                     class = "peekbank_connection")
    expect_warning(
      value <- peekbankr:::cached_read(con, "vtest unsaved", function() 42),
      "Could not save")
    expect_equal(value, 42)
  })
})

test_that("clear_cache deletes this version's files and no others", {
  downloads <- fake_redivis()
  in_fresh_dir({
    pb <- peekbank("2026.1", cache_on_disk = TRUE)
    invisible(pb$datasets)
    other <- file.path(".pbcache", "vother", "entry.rds")
    dir.create(dirname(other))
    file.create(other)

    pb$clear_cache()
    expect_false(dir.exists(file.path(".pbcache", "vtest")))
    expect_true(file.exists(other))

    invisible(pb$datasets)
    expect_equal(downloads$n, 2)
  })
})

test_that("the draft of the next release cannot be cached on disk", {
  in_fresh_dir({
    expect_error(peekbank("next", cache_on_disk = TRUE), "cannot be used")
    expect_false(dir.exists(".pbcache"))
  })
})

test_that("the cache stays put after setwd() and keeps itself out of git", {
  fake_redivis()
  in_fresh_dir({
    project <- getwd()
    pb <- peekbank("2026.1", cache_on_disk = TRUE)
    in_fresh_dir(invisible(pb$datasets))
    expect_length(list.files(file.path(project, ".pbcache", "vtest")), 1)
    expect_equal(readLines(file.path(project, ".pbcache", ".gitignore")), "*")
  })
})

test_that("a timepoint subset is served from disk without downloading it", {
  skip_network()
  in_fresh_dir({
    pb <- suppressMessages(peekbank("current", cache_on_disk = TRUE))
    subset <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")

    forget_session(pb$redivis_tag)
    local_mocked_bindings(pb_dataset = function(...) stop("downloaded"))
    expect_identical(
      pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016"), subset)
  })
})
