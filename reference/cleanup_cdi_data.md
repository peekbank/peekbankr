# Checks cdi data for inconsistencies, warns about them, and fixes them

**\[superseded\]**

Peekbank allows more than one score for the same instrument, measure and
age, so decide per analysis which to keep, e.g. with
`dplyr::slice_max(rawscore, by = c(subject_id, instrument_type, measure, age))`.

## Usage

``` r
cleanup_cdi_data(cdi_data)
```

## Arguments

- cdi_data:

  a subjects table with unnested cdi data, needs columns "subject_id",
  "language", "instrument_type", "age", "sex", "measure", "rawscore"

## Value

a cleaned up version of the cdi data

## Examples

``` r
if (FALSE) { # \dontrun{
clean_cdi_data <- all_subjects %>%
  unnest(subject_aux_data) %>%
  filter(!is.na(cdi_responses)) %>%
  unnest(cdi_responses) %>%
  peekbankr::cleanup_cdi_data()
} # }
```
