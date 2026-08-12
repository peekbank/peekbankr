# Characterization: the Redivis backend must reproduce fixtures captured from
# the retired MySQL backend across the full get_* argument matrix.
#
# Run with:
#   PEEKBANK_FIXTURES_DIR=path/to/migration/fixtures \
#     Rscript -e 'testthat::test_local(filter = "characterization")'

char_versions <- function() {
  intersect(c("2021.1", "2025.1", "2026.1"),
            list.dirs(char_fixtures_dir(), recursive = FALSE,
                      full.names = FALSE))
}

test_that("redivis backend reproduces the MySQL fixtures", {
  skip_characterization()
  for (v in char_versions()) {
    con <- suppressMessages(connect_to_peekbank(db_version = v))
    for (name in names(char_calls)) {
      path <- file.path(char_fixtures_dir(), v, paste0(name, ".rds"))
      if (!file.exists(path)) next
      result <- char_calls[[name]](con)
      withCallingHandlers(
        expect_matches_fixture(result, path),
        expectation_failure = function(e) {
          message("characterization failure at ", v, "/", name)
        }
      )
    }
  }
})

test_that("table listing matches modulo documented staging changes", {
  skip_characterization()
  for (v in char_versions()) {
    lt_path <- file.path(char_fixtures_dir(), v, "list_tables.rds")
    if (!file.exists(lt_path)) next
    con <- suppressMessages(connect_to_peekbank(db_version = v))
    # Redivis versions add release_info (documented) and drop the django
    # bookkeeping tables + the rebuildable aoi_timepoints_indexed intermediate
    old_tables <- setdiff(readRDS(lt_path)$table,
                          c("admin", "django_migrations",
                            "aoi_timepoints_indexed"))
    new_tables <- setdiff(list_peekbank_tables(con), "release_info")
    expect_setequal(new_tables, old_tables)
  }
})

test_that("aux data unpacking matches the fixture", {
  skip_characterization()
  skip_if(!"2026.1" %in% char_versions())
  aux_path <- file.path(char_fixtures_dir(), "2026.1",
                        "subjects_aux_unpacked.rds")
  skip_if(!file.exists(aux_path))
  con <- suppressMessages(connect_to_peekbank(db_version = "2026.1"))
  result <- unpack_aux_data(get_subjects(connection = con))
  fixture <- readRDS(aux_path)

  f <- tidyr::unnest(fixture, "subject_aux_data")
  r <- tidyr::unnest(result, "subject_aux_data")

  # unpack_aux_data derives its column order from data row order, which is
  # not part of the API contract (BigQuery result order differs from MySQL
  # PK order) -- compare order-insensitively, and compare list columns
  # (cdi_responses etc.) element-wise after aligning rows on subject_id
  expect_setequal(names(r), names(f))
  r <- r[names(f)]
  f <- f[order(f$subject_id), ]
  r <- r[order(r$subject_id), ]
  expect_equal(nrow(r), nrow(f))

  is_list_col <- vapply(f, is.list, logical(1))
  expect_true(isTRUE(all.equal(
    as.data.frame(f[!is_list_col]), as.data.frame(r[!is_list_col]),
    tolerance = 1e-8, check.attributes = FALSE)))
  for (col in names(f)[is_list_col]) {
    same <- mapply(function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8,
                                                   check.attributes = FALSE)),
                   f[[col]], r[[col]])
    expect_true(all(same), label = paste0("list column '", col, "' matches"))
  }
})
