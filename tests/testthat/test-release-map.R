# offline unit tests for version discovery

fake_version <- function(tag, index, released = TRUE) {
  list(properties = list(tag = tag, index = index, isReleased = released))
}

fake_dataset <- function(current, versions) {
  function(tag = NULL) {
    list(
      get = function() list(properties = list(version = list(tag = current))),
      list_versions = function() versions
    )
  }
}

test_that("release_map reads the version line rather than deriving tags", {
  .peekbankr_env$release_map <- NULL
  on.exit(.peekbankr_env$release_map <- NULL)

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
    release_name_at = function(tag) unname(releases[tag])
  )

  rm_ <- release_map()
  expect_equal(rm_$current_tag, "v2.0")
  expect_equal(rm_$map,
               c("2021.0" = "v2.0", "2022.1" = "v1.1", "2021.1" = "v1.0"))
  expect_false("next" %in% rm_$map)
})

test_that("release_map drops versions whose release_info cannot be read", {
  .peekbankr_env$release_map <- NULL
  on.exit(.peekbankr_env$release_map <- NULL)

  versions <- list(fake_version("v1.0", 0), fake_version("v1.1", 1))

  local_mocked_bindings(
    redivis_available = function() TRUE,
    pb_dataset = fake_dataset("v1.1", versions),
    release_name_at = function(tag) if (tag == "v1.1") "2022.1" else NULL
  )

  expect_equal(release_map()$map, c("2022.1" = "v1.1"))
})
