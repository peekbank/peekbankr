# Get subjects

Get subjects

## Usage

``` r
get_subjects(connection = NULL)
```

## Arguments

- connection:

  A connection to the peekbank database

## Value

A tibble of Subjects data. Note that Subjects is a table used to link
longitudinal Administrations, which is the primary table you probably
want.

## Examples

``` r
if (FALSE) { # \dontrun{
get_subjects()
} # }
```
