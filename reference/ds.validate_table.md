# Check if a dataframe/table is compliant to peekbank json before database import

Check if a dataframe/table is compliant to peekbank json before database
import

## Usage

``` r
ds.validate_table(
  df_table,
  table_type,
  cdi_expected,
  dir_csv,
  is_null_field_required = TRUE
)
```

## Arguments

- df_table:

  the dataframe to be saved

- table_type:

  the type of dataframe, for the most updated table types specified by
  schema, please use ds.list_ds_tables()

- cdi_expected:

  specifies whether cdi_data is to be expected to be present in the
  imported data; only relevant for subjects table

- dir_csv:

  the folder directory containing all the csv files, used for stimulus
  image path validation

- is_null_field_required:

  by default is set to TRUE which means that all the columns in the json
  file are required; when user specifically sets this to FALSE, then the
  fields that are allowed null values are not required.

## Value

A list with two elements:

- errors:

  Character vector of validation errors (blocking), or NULL if none.

- warnings:

  Named list of validation warnings (suppressable). Names are warning
  IDs (e.g. `"cdi_collision"`).

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.validate_table(df_table = df_table, table_type = "xy_data",
                            cdi_expected = FALSE,
                            dir_csv = "../processed_data")
result$errors    # blocking issues
result$warnings  # warnings that can be opted out of on a case by case basis
} # }
```
