# Get stimuli

Get stimuli

## Usage

``` r
get_stimuli(dataset_id = NULL, dataset_name = NULL, connection = NULL)
```

## Arguments

- dataset_id:

  An integer vector of one or more dataset ids

- dataset_name:

  A character vector of one or more dataset names

- connection:

  A connection to the peekbank database

## Value

A tibble of Stimuli data, filtered down by supplied arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
get_stimuli()
get_stimuli(dataset_name = "pomper_saffran_2016")
} # }
```
