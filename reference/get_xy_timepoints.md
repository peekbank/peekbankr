# Get XY timepoints

Get XY timepoints

## Usage

``` r
get_xy_timepoints(
  dataset_id = NULL,
  dataset_name = NULL,
  age = NULL,
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

- connection:

  A connection to the peekbank database

## Value

A tibble of XY timepoints data, filtered down by supplied arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
get_xy_timepoints(dataset_name = "reflook_v4")
} # }
```
