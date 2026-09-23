# Get administrations

Get administrations

## Usage

``` r
get_administrations(
  age = NULL,
  dataset_id = NULL,
  dataset_name = NULL,
  connection = NULL
)
```

## Arguments

- age:

  A numeric vector of a single age or a min age and max age (inclusive),
  in months

- dataset_id:

  An integer vector of one or more dataset ids

- dataset_name:

  A character vector of one or more dataset names

- connection:

  A connection to the peekbank database

## Value

A tibble of Administrations data, filtered down by supplied arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
get_administrations()
get_administrations(age = c())
get_administrations(dataset_name = "pomper_saffran_2016")
} # }
```
