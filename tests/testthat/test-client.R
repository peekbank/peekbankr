test_that("every table is a field, a subset function and a db handle", {
  tables <- c("datasets", "subjects", "administrations", "trials",
              "trial_types", "stimuli", "aoi_region_sets", "aoi_timepoints",
              "xy_timepoints")
  expect_true(all(tables %in% names(PeekbankClient$active)))
  expect_true(all(c("query", "images", "readmes", "clear_cache") %in%
                    names(PeekbankClient$public_methods)))
  # tables are fields, not methods, so neither spelling can shadow the other
  expect_false(any(tables %in% names(PeekbankClient$public_methods)))
})

test_that("the client is pinned and cannot be re-pointed", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  expect_s3_class(pb, "PeekbankClient")
  expect_match(pb$release, "^\\d{4}\\.\\d+$")
  expect_match(pb$redivis_tag, "^v\\d+\\.\\d+$")
  expect_error(pb$redivis_tag <- "v1.0", "read-only")
  expect_error(pb$release <- "1999.1", "read-only")
})

test_that("peekbank() rejects unknown versions", {
  skip_network()
  expect_error(suppressMessages(peekbank("1999.9")), "not found")
  # a version is named by its release. A Redivis tag is the storage detail
  # underneath, and reads backwards: v2.0 holds the 2021.0 release.
  expect_error(suppressMessages(peekbank("v1.4")), "not found")
  expect_error(suppressMessages(connect_to_peekbank("v1.4")), "not found")
})

test_that("client methods return the same data as the bare functions", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  con <- suppressMessages(connect_to_peekbank(pb$release))
  expect_equal(pb$datasets, get_datasets(connection = con))
  expect_equal(pb$tables, list_peekbank_tables(con))
})

test_that("two clients can hold different versions at once", {
  skip_network()
  releases <- names(get_db_info()$versions)
  skip_if(length(releases) < 2, "need two releases to compare")
  a <- suppressMessages(peekbank(releases[1]))
  b <- suppressMessages(peekbank(releases[2]))
  expect_false(identical(a$redivis_tag, b$redivis_tag))
  expect_equal(a$release, releases[1])
  expect_equal(b$release, releases[2])
})

test_that("printing names the release and the three tiers", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  expect_output(print(pb), "<peekbank> release")
  expect_output(print(pb), "tables:")
  expect_output(print(pb), "aoi_timepoints")
  expect_output(print(pb), "\\$subset\\$")
  expect_output(print(pb), "\\$db\\$")
})

test_that("clear_cache drops this version's entries and leaves others alone", {
  skip_network()
  env <- peekbankr:::.peekbankr_env
  pb <- suppressMessages(peekbank("current"))
  invisible(pb$datasets)
  expect_false(is.null(env[[paste(pb$redivis_tag, "datasets")]]))

  other <- "zzz_not_a_real_tag datasets"
  env[[other]] <- data.frame(x = 1)
  on.exit(rm(list = other, envir = env), add = TRUE)

  pb$clear_cache()
  expect_null(env[[paste(pb$redivis_tag, "datasets")]])
  # another version's cache, and the version-resolution metadata, survive
  expect_false(is.null(env[[other]]))
  expect_false(is.null(env$published_redivis_tags))
})

test_that("db fields give dbplyr tables scoped to this client's version", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  expect_s3_class(pb$db, "peekbank_db_tables")
  expect_true(all(c("administrations", "aoi_timepoints") %in% ls(pb$db)))
  expect_error(pb$db <- 1, "read-only")
  t <- pb$db$datasets
  expect_s3_class(t, "tbl_lazy")

  # the version tag is baked into the table path, with dots as underscores
  sql <- as.character(dbplyr::sql_render(t))
  expect_match(sql, gsub(".", "_", pb$redivis_tag, fixed = TRUE), fixed = TRUE)

  # dplyr verbs compile to SQL rather than fetching
  agg <- t |> dplyr::count(.data$dataset_name)
  expect_match(as.character(dbplyr::sql_render(agg)), "GROUP BY")

  # one DBI connection per client, reused
  expect_identical(pb$db$datasets$src$con, pb$db$trials$src$con)
})

test_that("db handles show the same columns as the fields", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  for (nm in setdiff(peekbankr:::peekbank_tables,
                     c("aoi_timepoints", "xy_timepoints"))) {
    expect_identical(suppressMessages(colnames(pb$db[[nm]])), names(pb[[nm]]),
                     info = nm)
  }
})

test_that("dataset_name filters a db handle the same as subset", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  ds <- "pomper_saffran_2016"
  collected <- pb$db$administrations |>
    dplyr::filter(.data$dataset_name == ds) |>
    dplyr::collect() |>
    dplyr::arrange(.data$administration_id)
  expect_equal(collected, pb$subset$administrations(dataset_name == ds) |>
                 dplyr::arrange(.data$administration_id),
               ignore_attr = TRUE)
})

test_that("db lists the stored tables but not the version plumbing", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  expect_true("aoi_timepoints_rle" %in% ls(pb$db))
  expect_false("release_info" %in% ls(pb$db))
})

test_that("timepoint predicates are translated into the query", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  sql <- captured_sql(
    pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016", age < 24))

  # the run-length-encoded table is the one fetched by default
  expect_match(sql, "aoi_timepoints_rle", fixed = TRUE)
  # predicates are translated, not interpolated as ids
  expect_match(sql, "'pomper_saffran_2016'", fixed = TRUE)
  expect_match(sql, "`age` < 24", fixed = TRUE)
  # the decode relies on run order
  expect_match(sql, "ORDER BY `administration_id`, `trial_id`, `t_norm`",
               fixed = TRUE)
})

test_that("predicates reach the trial a timepoint belongs to", {
  skip_network()
  # pinned: excluded and vanilla_trial arrived in 2025.1
  pb <- suppressMessages(peekbank("2026.1"))
  # columns of trials, of trial_types, and of either stimulus
  sql <- captured_sql(pb$subset$aoi_timepoints(
    !excluded, vanilla_trial, target_stimulus_novelty == "familiar",
    distractor_english_stimulus_label != "dog"))
  # newer dbplyr qualifies the column with the subquery alias
  expect_match(sql, "NOT\\((`[^`]+`\\.)?`excluded`\\)")
  expect_match(sql, "'familiar'", fixed = TRUE)
  expect_match(sql, "`distractor_english_stimulus_label`", fixed = TRUE)
  # a column none of those tables has is an error
  expect_error(pb$subset$aoi_timepoints(not_a_column == 1))
})

test_that("a predicate can span the administration and the trial", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  ds <- "pomper_saffran_2016"
  either <- pb$subset$aoi_timepoints(dataset_name == ds,
                                     age < 20 | !vanilla_trial)
  young <- pb$subset$aoi_timepoints(dataset_name == ds, age < 20)
  odd <- pb$subset$aoi_timepoints(dataset_name == ds, !vanilla_trial)
  # a union, so at least either half and at most both
  expect_gte(nrow(either), max(nrow(young), nrow(odd)))
  expect_lte(nrow(either), nrow(young) + nrow(odd))
})

test_that("rle = FALSE reads the expanded table instead", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  sql <- captured_sql(pb$subset$aoi_timepoints(age < 24, rle = FALSE))
  expect_false(grepl("aoi_timepoints_rle", sql, fixed = TRUE))
  expect_match(sql, "aoi_timepoints", fixed = TRUE)
})

test_that("a named argument is caught with a hint", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  msg <- tryCatch(
    pb$subset$aoi_timepoints(dataset_name = "pomper_saffran_2016"),
    error = conditionMessage)
  expect_match(msg, "`=` instead of `==`", fixed = TRUE)
  expect_match(msg, "dataset_name", fixed = TRUE)
})

test_that("no predicates reads the table without joining anything", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  sql <- captured_sql(pb$subset$aoi_timepoints())
  expect_false(grepl("JOIN", sql, fixed = TRUE))
  expect_false(grepl("WHERE", sql, fixed = TRUE))
})

test_that("a trial predicate narrows to the same rows as filtering after", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  ds <- "pomper_saffran_2016"
  server <- pb$subset$aoi_timepoints(dataset_name == ds, vanilla_trial,
                                     !excluded)
  local <- pb$subset$aoi_timepoints(dataset_name == ds) |>
    dplyr::inner_join(dplyr::select(pb$trials,
                                    dplyr::all_of(c("trial_id",
                                                    "trial_type_id",
                                                    "excluded"))),
                      by = "trial_id") |>
    dplyr::inner_join(dplyr::select(pb$trial_types,
                                    dplyr::all_of(c("trial_type_id",
                                                    "vanilla_trial"))),
                      by = "trial_type_id") |>
    dplyr::filter(.data$vanilla_trial, !.data$excluded) |>
    dplyr::select(dplyr::all_of(names(server)))
  expect_equal(nrow(server), nrow(local))
  expect_equal(dplyr::arrange(server, .data$administration_id, .data$trial_id,
                              .data$t_norm),
               dplyr::arrange(local, .data$administration_id, .data$trial_id,
                              .data$t_norm),
               ignore_attr = TRUE)
})

test_that("a predicate naming no column says what can be named", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))

  msg <- tryCatch(pb$subset$trials(vanilla_trail), error = conditionMessage)
  expect_match(msg, "No column `vanilla_trail` to filter trials on",
               fixed = TRUE)
  # the list is the point: vanilla_trial is a trial_types column, so no
  # per-table documentation would have told you it can be named here
  expect_match(msg, "Can filter on:", fixed = TRUE)
  expect_match(msg, "vanilla_trial", fixed = TRUE)

  msg <- tryCatch(pb$subset$subjects(age < 24), error = conditionMessage)
  expect_match(msg, "No column `age` to filter subjects on", fixed = TRUE)

  # the encoded table is an implementation detail, so it is not named
  msg <- tryCatch(pb$subset$aoi_timepoints(vanila_trial),
                  error = conditionMessage)
  expect_match(msg, "filter aoi_timepoints on", fixed = TRUE)
  expect_false(grepl("aoi_timepoints_rle", msg, fixed = TRUE))
  # and the run columns it refuses are not offered either
  expect_false(grepl("length,", msg, fixed = TRUE))
})

test_that("predicates may still name locals and call functions", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  ds <- "pomper_saffran_2016"
  wanted <- c(ds, "adams_marchman_2018")

  expect_equal(nrow(pb$subset$trials(dataset_name == ds)),
               nrow(pb$subset$trials(dataset_name == "pomper_saffran_2016")))
  expect_s3_class(pb$subset$administrations(dataset_name %in% wanted),
                  "data.frame")
  expect_s3_class(pb$subset$subjects(!is.na(sex)), "data.frame")
  expect_s3_class(pb$subset$administrations(age < (12 * 2)), "data.frame")
  expect_s3_class(pb$subset$administrations(.data$age < 24), "data.frame")
})

test_that("t_norm filters samples, not runs, on the encoded table", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  ds <- "pomper_saffran_2016"
  expect_error(pb$subset$aoi_timepoints(length > 5), "No column `length`")

  # the expanded table has one row per sample, so there t_norm is exact
  expect_equal(
    pb$subset$aoi_timepoints(dataset_name == ds, t_norm > 0),
    pb$subset$aoi_timepoints(dataset_name == ds, t_norm > 0, rle = FALSE),
    ignore_attr = TRUE)

  # a predicate mixing t_norm with a column the result does not have
  mixed <- pb$subset$aoi_timepoints(dataset_name == ds,
                                    t_norm > 1000 | !vanilla_trial)
  expect_named(mixed, c("administration_id", "trial_id", "aoi", "t_norm"))
  expect_equal(
    mixed,
    pb$subset$aoi_timepoints(dataset_name == ds,
                             t_norm > 1000 | !vanilla_trial, rle = FALSE),
    ignore_attr = TRUE)

  # aoi is constant within a run, so it is safe to push down
  expect_equal(
    nrow(pb$subset$aoi_timepoints(dataset_name == ds, aoi == "target")),
    nrow(dplyr::filter(pb$subset$aoi_timepoints(dataset_name == ds),
                       .data$aoi == "target")))
})

test_that("predicates reach the whole dataset, not just its name", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  ds <- "pomper_saffran_2016"
  cite <- pb$datasets$shortcite[pb$datasets$dataset_name == ds]
  expect_equal(nrow(pb$subset$aoi_timepoints(shortcite == !!cite)),
               nrow(pb$subset$aoi_timepoints(dataset_name == ds)))
})

test_that("xy_timepoints has no run-length encoding to ask about", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  expect_false("rle" %in% names(formals(pb$subset$xy_timepoints)))
  expect_true("rle" %in% names(formals(pb$subset$aoi_timepoints)))
})

test_that("the RLE decode reproduces the stored expanded table", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  shared <- c("administration_id", "trial_id", "t_norm", "aoi")
  ordered <- function(d) {
    d <- d[, shared]
    as.data.frame(d[order(d$administration_id, d$trial_id, d$t_norm), ])
  }
  decoded <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")
  stored <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016",
                                     rle = FALSE)
  expect_equal(ordered(decoded), ordered(stored), ignore_attr = TRUE)

  # the encoding is an implementation detail, so it cannot change the columns
  expect_identical(names(decoded), names(stored))
  expect_identical(names(decoded),
                   suppressMessages(colnames(pb$db$aoi_timepoints)))
})

test_that("predicates give the same rows as the legacy getter", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  con <- suppressMessages(connect_to_peekbank("2026.1"))
  expect_equal(
    pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016"),
    get_aoi_timepoints(dataset_name = "pomper_saffran_2016", connection = con))
})

test_that("the bare fields match the legacy getters", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  con <- suppressMessages(connect_to_peekbank("2026.1"))
  expect_equal(pb$datasets, get_datasets(connection = con))
  expect_equal(pb$administrations, get_administrations(connection = con))
  # the legacy getters keep excluded as 0/1
  expect_equal(pb$trials, peekbankr:::as_logical_flags(
    "trials", get_trials(connection = con)))
  expect_equal(pb$stimuli, get_stimuli(connection = con))
})

test_that("subset on a small table narrows the same rows as dplyr would", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  expect_equal(pb$subset$administrations(age < 24),
               dplyr::filter(pb$administrations, age < 24))
})

test_that("widening for a predicate duplicates no rows", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  for (nm in setdiff(peekbankr:::peekbank_tables,
                     c("aoi_timepoints", "xy_timepoints"))) {
    # with nothing to filter on, the widened frame must narrow back to the
    # table exactly: every table it joins has a unique key
    expect_equal(pb$subset[[nm]](), pb[[nm]], info = nm)
  }
})

test_that("a predicate can name what a row belongs to, on any table", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  # an administration belongs to a subject
  expect_equal(
    pb$subset$administrations(sex == "female"),
    dplyr::semi_join(pb$administrations,
                     dplyr::filter(pb$subjects, .data$sex == "female"),
                     by = "subject_id"))
  # a trial belongs to a trial type
  expect_equal(
    pb$subset$trials(vanilla_trial),
    dplyr::semi_join(pb$trials,
                     dplyr::filter(pb$trial_types, .data$vanilla_trial),
                     by = "trial_type_id"))
  # a trial type names two stimuli
  expect_equal(
    nrow(pb$subset$trial_types(target_stimulus_novelty == "novel")),
    nrow(dplyr::semi_join(pb$trial_types,
                          dplyr::filter(pb$stimuli,
                                        .data$stimulus_novelty == "novel"),
                          by = c("target_id" = "stimulus_id"))))
  # and a trial type uses an AOI region set
  expect_s3_class(pb$subset$trial_types(is.na(l_x_max)), "data.frame")
})

test_that("a mistyped name errors instead of giving NULL", {
  skip_network()
  pb <- suppressMessages(peekbank("current"))
  expect_error(pb$administrationss, "No field or method called")
  expect_error(pb[["administrationss"]], "No field or method called")
  expect_error(pb$db$administrationss, "No table called")
  expect_error(pb$subset$admin, "No table called")
  # the error says what is there instead, minus R6's constructor
  msg <- tryCatch(pb$nope, error = conditionMessage)
  expect_match(msg, "administrations", fixed = TRUE)
  expect_false(grepl("initialize", msg, fixed = TRUE))
})

test_that("peekbank_releases lists what peekbank() accepts", {
  skip_network()
  releases <- peekbank_releases()
  expect_named(releases, c("release", "redivis_tag", "current"))
  expect_gt(nrow(releases), 1)
  expect_equal(sum(releases$current), 1)
  # every release listed can actually be pinned
  pb <- suppressMessages(peekbank(releases$release[[1]]))
  expect_equal(pb$release, releases$release[[1]])
  expect_equal(pb$redivis_tag, releases$redivis_tag[[1]])
  # and "current" is the row flagged as such
  expect_equal(suppressMessages(peekbank("current"))$redivis_tag,
               releases$redivis_tag[releases$current])
})

test_that("peekbank() refuses to guess a version", {
  expect_error(peekbank(), "`version` is required")
  expect_error(PeekbankClient$new(), "`version` is required")
})

test_that("a predicate that does not translate says how to fix it", {
  skip_network()
  pb <- suppressMessages(peekbank("2026.1"))
  one <- pb$administrations$administration_id[[1]]
  msg <- tryCatch(
    pb$subset$aoi_timepoints(administration_id == one, not_a_function(aoi)),
    error = conditionMessage)
  # BigQuery's own complaint, then the R-side cause and where to look
  expect_match(msg, "not_a_function", fixed = TRUE)
  expect_match(msg, "translated to SQL", fixed = TRUE)
  expect_match(msg, "docs.redivis.com", fixed = TRUE)
})
