# Package index

## All functions

- [`append_relative_cdi_scores()`](https://peekbank.github.io/peekbankr/reference/append_relative_cdi_scores.md)
  : Adds a relative cdi score indicating the percentage of total
  achievable points the subject got on each given measure
- [`cleanup_cdi_data()`](https://peekbank.github.io/peekbankr/reference/cleanup_cdi_data.md)
  : Checks cdi data for inconsistencies, warns about them, and fixes
  them
- [`connect_to_peekbank()`](https://peekbank.github.io/peekbankr/reference/connect_to_peekbank.md)
  : Connect to Peekbank
- [`download_stimuli()`](https://peekbank.github.io/peekbankr/reference/download_stimuli.md)
  : Download stimulus images for the Peekbank repository
- [`ds.compute_aois()`](https://peekbank.github.io/peekbankr/reference/ds.compute_aois.md)
  : Classify gaze samples into AOIs based x/y coordinates and provided
  region sets
- [`ds.get_json_fields()`](https://peekbank.github.io/peekbankr/reference/ds.get_json_fields.md)
  : Fetching the list of field names and requirements in each table
  according to the schema json file
- [`ds.get_peekjson()`](https://peekbank.github.io/peekbankr/reference/ds.get_peekjson.md)
  : parse the bundled peekbank schema json into a dataframe
- [`ds.is_table_required()`](https://peekbank.github.io/peekbankr/reference/ds.is_table_required.md)
  : Check if a certain table is required according to schema
- [`ds.list_coding_methods()`](https://peekbank.github.io/peekbankr/reference/ds.list_coding_methods.md)
  : Get the coding method list from json schema file
- [`ds.list_ds_tables()`](https://peekbank.github.io/peekbankr/reference/ds.list_ds_tables.md)
  : List the tables required based on coding method
- [`ds.list_language_choices()`](https://peekbank.github.io/peekbankr/reference/ds.list_language_choices.md)
  : List current allowed language choices for db import
- [`ds.map_columns()`](https://peekbank.github.io/peekbankr/reference/ds.map_columns.md)
  : Function for mapping raw data columns to processed table columns
- [`ds.normalize_times()`](https://peekbank.github.io/peekbankr/reference/ds.normalize_times.md)
  : sets the starting point of a given trial to be zero
- [`ds.resample_times()`](https://peekbank.github.io/peekbankr/reference/ds.resample_times.md)
  : This function resample times to be consistent across labs.
- [`ds.rezero_times()`](https://peekbank.github.io/peekbankr/reference/ds.rezero_times.md)
  : sets the starting point of a given trial to be zero
- [`ds.validate_for_db_import()`](https://peekbank.github.io/peekbankr/reference/ds.validate_for_db_import.md)
  : check all csv files against database schema for database import
- [`ds.validate_table()`](https://peekbank.github.io/peekbankr/reference/ds.validate_table.md)
  : Check if a dataframe/table is compliant to peekbank json before
  database import
- [`ds.validate_trial_uniqueness_constraint()`](https://peekbank.github.io/peekbankr/reference/ds.validate_trial_uniqueness_constraint.md)
  : Check if within aoi_timepoints table, there is no duplication in all
  the administration_ids associated with each individual trial_id
- [`file.exists.case.sensitive()`](https://peekbank.github.io/peekbankr/reference/file.exists.case.sensitive.md)
  : Check if a file exists with exact case sensitivity
- [`get_administrations()`](https://peekbank.github.io/peekbankr/reference/get_administrations.md)
  : Get administrations
- [`get_aoi_region_sets()`](https://peekbank.github.io/peekbankr/reference/get_aoi_region_sets.md)
  : Get AOI region sets
- [`get_aoi_timepoints()`](https://peekbank.github.io/peekbankr/reference/get_aoi_timepoints.md)
  : Get AOI timepoints
- [`get_datasets()`](https://peekbank.github.io/peekbankr/reference/get_datasets.md)
  : Get datasets
- [`get_db_info()`](https://peekbank.github.io/peekbankr/reference/get_db_info.md)
  : Get information on database connection options
- [`get_readmes()`](https://peekbank.github.io/peekbankr/reference/get_readmes.md)
  : Download dataset README files
- [`get_sql_query()`](https://peekbank.github.io/peekbankr/reference/get_sql_query.md)
  : Run a SQL query against peekbank
- [`get_stimuli()`](https://peekbank.github.io/peekbankr/reference/get_stimuli.md)
  : Get stimuli
- [`get_subjects()`](https://peekbank.github.io/peekbankr/reference/get_subjects.md)
  : Get subjects
- [`get_trial_types()`](https://peekbank.github.io/peekbankr/reference/get_trial_types.md)
  : Get trial types
- [`get_trials()`](https://peekbank.github.io/peekbankr/reference/get_trials.md)
  : Get trials
- [`get_xy_timepoints()`](https://peekbank.github.io/peekbankr/reference/get_xy_timepoints.md)
  : Get XY timepoints
- [`list_peekbank_tables()`](https://peekbank.github.io/peekbankr/reference/list_peekbank_tables.md)
  : List of peekbank tables
- [`populate_cdi_percentiles()`](https://peekbank.github.io/peekbankr/reference/populate_cdi_percentiles.md)
  : Populate the provided cdi data with percentile values for that
  specific age, instrument_type, measure and language. Loosely based on
  the work from this repo
  https://github.com/kachergis/cdi-percentiles/tree/main by George
  Kachergis and Jess Mankewitz with advice from Virginia Marchman.
- [`unpack_aux_data()`](https://peekbank.github.io/peekbankr/reference/unpack_aux_data.md)
  : Unpack the json sting in the \*\_aux_data column and turns it into a
  nested R list
