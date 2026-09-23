# Get trials

Get trials

## Usage

``` r
get_trials(dataset_id = NULL, dataset_name = NULL, connection = NULL)
```

## Arguments

- dataset_id:

  An integer vector of one or more dataset ids

- dataset_name:

  A character vector of one or more dataset names

- connection:

  A connection to the peekbank database

## Value

A tibble of Trials data, filtered down by supplied arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
get_trials()
get_trials(dataset_name = "pomper_saffran_2016")
} # }
```
