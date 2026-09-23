# check all csv files against database schema for database import

check all csv files against database schema for database import

## Usage

``` r
ds.validate_for_db_import(
  dir_csv,
  cdi_expected,
  file_ext = ".csv",
  is_null_field_required = TRUE,
  suppress_warnings = c()
)
```

## Arguments

- dir_csv:

  the folder directory containing all the csv files, the path should end
  in "processed_data"

- cdi_expected:

  specifies whether cdi_data is to be expected to be present in the
  imported data

- file_ext:

  the default is ".csv"

- is_null_field_required:

  by default is set to TRUE which means that all the columns in the json
  file are required; when set to FALSE, fields that are allowed null
  values are not required

- suppress_warnings:

  character vector of warning IDs to silence. Currently supported:
  `"cdi_collision"` (duplicate CDI entries for the same
  subject/instrument/measure/age/language) and
  `"stimulus_image_filetype"` (stimulus images in a format other than
  jpg/jpeg/png).

## Value

A list with two elements:

- errors:

  Character vector of validation errors (blocking), or NULL if none.

- warnings:

  Character vector of validation warnings (suppressible), or NULL if
  none. Warnings matching `suppress_warnings` are excluded.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.validate_for_db_import(dir_csv = "./processed_data", cdi_expected = TRUE)
result$errors    # blocking issues
result$warnings  # warnings that can be opted out of on a case by case basis

# suppress known warnings for a specific dataset
result <- ds.validate_for_db_import(dir_csv = "./processed_data", cdi_expected = TRUE,
                                    suppress_warnings = c("cdi_collision"))
} # }
```
