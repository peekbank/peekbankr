# Get trial types

Get trial types

## Usage

``` r
get_trial_types(dataset_id = NULL, dataset_name = NULL, connection = NULL)
```

## Arguments

- dataset_id:

  An integer vector of one or more dataset ids

- dataset_name:

  A character vector of one or more dataset names

- connection:

  A connection to the peekbank database

## Value

A tibble of Trial Types data, filtered down by supplied arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
get_trial_types()
get_trial_types(dataset_name = "pomper_saffran_2016")
} # }
```
