# helpers for the characterization tests (fixtures captured from the retired
# MySQL backend; see peekbank-datapage/migration/capture_fixtures.R)

char_fixtures_dir <- function() Sys.getenv("PEEKBANK_FIXTURES_DIR")

skip_characterization <- function() {
  skip_on_cran()
  skip_if_not_installed("redivis")
  skip_if(char_fixtures_dir() == "", "PEEKBANK_FIXTURES_DIR not set")
}

# row order is not part of the API (MySQL returned PK/insert order, BigQuery
# is unordered) -> sort by all columns; integer64 (RMariaDB bigint) vs
# integer/double (arrow) is not a semantic difference -> compare as double
char_normalize <- function(df) {
  df <- as.data.frame(df)
  df[] <- lapply(df, function(col) {
    if (inherits(col, "integer64")) as.numeric(col)
    else if (is.integer(col)) as.numeric(col)
    else col
  })
  if (nrow(df) > 0) {
    df <- df[do.call(order, c(as.list(df), list(method = "radix"))), ,
             drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

expect_matches_fixture <- function(result, fixture_path) {
  fixture <- readRDS(fixture_path)
  f <- char_normalize(fixture)
  r <- char_normalize(result)
  expect_identical(names(r), names(f))
  expect_equal(nrow(r), nrow(f))
  expect_true(isTRUE(all.equal(f, r, tolerance = 1e-8,
                               check.attributes = FALSE)))
}

char_calls <- list(
  datasets = function(con) get_datasets(connection = con),
  subjects = function(con) get_subjects(connection = con),
  administrations = function(con) get_administrations(connection = con),
  administrations_age = function(con)
    get_administrations(age = c(18, 24), connection = con),
  administrations_ds = function(con)
    get_administrations(dataset_name = "pomper_saffran_2016", connection = con),
  trials = function(con) get_trials(connection = con),
  trials_ds = function(con)
    get_trials(dataset_name = "pomper_saffran_2016", connection = con),
  trial_types = function(con) get_trial_types(connection = con),
  trial_types_ds = function(con)
    get_trial_types(dataset_name = "pomper_saffran_2016", connection = con),
  stimuli = function(con) get_stimuli(connection = con),
  stimuli_ds = function(con)
    get_stimuli(dataset_name = "pomper_saffran_2016", connection = con),
  aoi_region_sets = function(con) get_aoi_region_sets(connection = con),
  aoi_timepoints_ps_rle = function(con)
    get_aoi_timepoints(dataset_name = "pomper_saffran_2016", connection = con),
  aoi_timepoints_ps_norle = function(con)
    get_aoi_timepoints(dataset_name = "pomper_saffran_2016", rle = FALSE,
                       connection = con),
  aoi_timepoints_ps_age = function(con)
    get_aoi_timepoints(dataset_name = "pomper_saffran_2016", age = c(41, 44),
                       connection = con),
  aoi_timepoints_sa = function(con)
    get_aoi_timepoints(dataset_name = "swingley_aslin_2002", connection = con),
  xy_timepoints_rv4 = function(con)
    get_xy_timepoints(dataset_name = "reflook_v4", connection = con),
  sql_query = function(con)
    get_sql_query("SELECT dataset_id, dataset_name FROM datasets",
                  connection = con)
)
