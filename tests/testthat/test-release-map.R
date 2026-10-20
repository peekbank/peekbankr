# offline unit tests for version discovery

fake_version <- function(redivis_tag, index, released = TRUE) {
  list(properties = list(tag = redivis_tag, index = index,
                         isReleased = released))
}

fake_dataset <- function(current, versions) {
  function(redivis_tag = NULL) {
    list(
      get = function() list(properties = list(version = list(tag = current))),
      list_versions = function() versions
    )
  }
}

test_that("releases are read from the versions, not derived from tags", {
  .peekbankr_env$published_redivis_tags <- NULL
  on.exit(.peekbankr_env$published_redivis_tags <- NULL)

  versions <- list(
    fake_version("v1.0", 0),
    fake_version("v1.1", 1),
    fake_version("v2.0", 2),
    fake_version("next", 3, released = FALSE)
  )
  releases <- c("v1.0" = "2021.1", "v1.1" = "2022.1", "v2.0" = "2021.0")

  local_mocked_bindings(
    redivis_available = function() TRUE,
    pb_dataset = fake_dataset("v2.0", versions),
    release_name_at = function(redivis_tag) unname(releases[redivis_tag])
  )

  releases <- peekbank_releases()
  expect_equal(releases$release, c("2021.0", "2022.1", "2021.1"))
  expect_equal(releases$redivis_tag, c("v2.0", "v1.1", "v1.0"))
  expect_equal(releases$current, c(TRUE, FALSE, FALSE))
  # the unreleased "next" version an editor can see is left out
  expect_false("next" %in% releases$redivis_tag)
})

test_that("a version whose release_info cannot be read is left out", {
  .peekbankr_env$published_redivis_tags <- NULL
  on.exit(.peekbankr_env$published_redivis_tags <- NULL)

  versions <- list(fake_version("v1.0", 0), fake_version("v1.1", 1))

  local_mocked_bindings(
    redivis_available = function() TRUE,
    pb_dataset = fake_dataset("v1.1", versions),
    release_name_at = function(redivis_tag) {
      if (redivis_tag == "v1.1") "2022.1" else NULL
    }
  )

  releases <- peekbank_releases()
  expect_equal(releases$release, "2022.1")
  expect_equal(releases$redivis_tag, "v1.1")
})

test_that("resolving a release stops at the first version that matches", {
  .peekbankr_env$published_redivis_tags <- NULL
  on.exit(.peekbankr_env$published_redivis_tags <- NULL)

  versions <- list(
    fake_version("v1.0", 0),
    fake_version("v1.1", 1),
    fake_version("v2.0", 2)
  )
  releases <- c("v1.0" = "2021.1", "v1.1" = "2022.1", "v2.0" = "2021.0")
  read <- character()

  local_mocked_bindings(
    redivis_available = function() TRUE,
    pb_dataset = fake_dataset("v2.0", versions),
    release_name_at = function(redivis_tag) {
      read <<- c(read, redivis_tag)
      unname(releases[redivis_tag])
    }
  )

  expect_equal(suppressMessages(resolve_release("2022.1"))$redivis_tag, "v1.1")
  # newest first, so the version older than the match is never opened
  expect_true("v2.0" %in% read)
  expect_false("v1.0" %in% read)
})

test_that("current needs only its own version", {
  .peekbankr_env$published_redivis_tags <- NULL
  on.exit(.peekbankr_env$published_redivis_tags <- NULL)

  versions <- list(fake_version("v1.0", 0), fake_version("v2.0", 1))
  read <- character()

  local_mocked_bindings(
    redivis_available = function() TRUE,
    pb_dataset = fake_dataset("v2.0", versions),
    release_name_at = function(redivis_tag) {
      read <<- c(read, redivis_tag)
      if (redivis_tag == "v2.0") "2021.0" else "2021.1"
    }
  )

  expect_equal(suppressMessages(resolve_release("current"))$release, "2021.0")
  expect_equal(unique(read), "v2.0")
})
