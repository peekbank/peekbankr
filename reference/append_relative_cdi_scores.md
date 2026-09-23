# Adds a relative cdi score indicating the percentage of total achievable points the subject got on each given measure

Adds a relative cdi score indicating the percentage of total achievable
points the subject got on each given measure

## Usage

``` r
append_relative_cdi_scores(subjects_table)
```

## Arguments

- subjects_table:

  a subjects table with unnested cdi data, needs columns "subject_id",
  "language", "instrument_type", "measure", "rawscore"

## Value

the input table with an added "cdi_relative" column that contains the
percentage of total points gained in the given administrations

## Examples

``` r
if (FALSE) { # \dontrun{
cdi_data <- all_subjects %>%
  unnest(subject_aux_data) %>%
  filter(!is.na(cdi_responses)) %>%
  unnest(cdi_responses) %>%
  append_relative_cdi_scores()
} # }
```
