# Populate the provided cdi data with percentile values for that specific age, instrument_type, measure and language. Loosely based on the work from this repo https://github.com/kachergis/cdi-percentiles/tree/main by George Kachergis and Jess Mankewitz with advice from Virginia Marchman.

Populate the provided cdi data with percentile values for that specific
age, instrument_type, measure and language. Loosely based on the work
from this repo https://github.com/kachergis/cdi-percentiles/tree/main by
George Kachergis and Jess Mankewitz with advice from Virginia Marchman.

## Usage

``` r
populate_cdi_percentiles(subjects_table)
```

## Arguments

- subjects_table:

  a subjects table with unnested cdi data, needs columns "subject_id",
  "language", "instrument_type", "age", "sex", "measure", "rawscore"

## Value

the input table with added columns containing the reference age used,
the reference year used, and both gender specific and general percentile
values for the cdi score

## Examples

``` r
if (FALSE) { # \dontrun{
full_cdi_data <- all_subjects %>%
  unnest(subject_aux_data) %>%
  filter(!is.na(cdi_responses)) %>%
  unnest(cdi_responses) %>%
  peekbankr::cleanup_cdi_data() %>%
  peekbankr::populate_cdi_percentiles()
} # }
```
