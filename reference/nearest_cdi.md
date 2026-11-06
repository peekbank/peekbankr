# Add the CDI scores closest in age

For each row of `df`, picks the child's CDI score closest in age in each
language, form and measure, among those at most `max_age_diff` months
away.

## Usage

``` r
nearest_cdi(df, cdi, max_age_diff, prefer_later = FALSE)
```

## Arguments

- df:

  A table with `subject_id` and `age`, e.g. `pb$administrations`

- cdi:

  CDI scores, one per row, e.g. from
  `unnest_aux(pb$subjects, "cdi_responses")`. Needs `subject_id`, `age`,
  `language`, `instrument_type` and `measure`.

- max_age_diff:

  The furthest a CDI score may be from the row's age, in months

- prefer_later:

  Of a score before and one after the row's age that are equally close,
  pick the later one instead of the earlier one

## Value

`df` with the columns of `cdi` added, one row per row of `df`, language,
form and measure, in the order of `df`. The score's age is `cdi_age`.
Columns `cdi` shares with `df`, like `dataset_id`, appear once and have
to hold the same values in both for each subject. Rows without a score
close enough are left out.

## Examples

``` r
if (FALSE) { # \dontrun{
pb <- peekbank("2026.1")
cdi <- unnest_aux(pb$subjects, "cdi_responses")
nearest_cdi(pb$administrations, cdi, max_age_diff = 3)
} # }
```
