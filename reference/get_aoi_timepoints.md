# Get AOI timepoints

Get AOI timepoints

## Usage

``` r
get_aoi_timepoints(
  dataset_id = NULL,
  dataset_name = NULL,
  age = NULL,
  rle = TRUE,
  connection = NULL
)
```

## Arguments

- dataset_id:

  An integer vector of one or more dataset ids

- dataset_name:

  A character vector of one or more dataset names

- age:

  A numeric vector of a single age or a min age and max age (inclusive),
  in months

- rle:

  Logical indicating whether to use RLE data representation or not

- connection:

  A connection to the peekbank database

## Value

A tibble of AOI Timepoints data, filtered down by supplied arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
get_aoi_timepoints(dataset_name = "pomper_saffran_2016")
} # }
```
