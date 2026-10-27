# Add vocabulary norm percentiles to CDI scores

Looks up where each raw score falls in the vocabulary norms for its
language, instrument, measure and age in months (rounded to the nearest
month). The norms cover English (American):

- WG and WG short form, comprehension and production, 8 to 18 months

- WS and WS short form, production, 16 to 30 months

## Usage

``` r
add_cdi_percentiles(cdi)
```

## Arguments

- cdi:

  CDI scores, one per row, e.g. from
  `unnest_aux(pb$subjects, "cdi_responses")`. Needs `language`,
  `instrument_type`, `measure`, `age` and `rawscore`; `sex` is used if
  present.

## Value

`cdi` with three columns added:

- `norm_age`: the age in months whose norms were used

- `norm_percentile`: the highest norm percentile (5, 10, ..., 95, 99)
  the score reaches, or 1 below the 5th

- `norm_percentile_sex`: the same against children of the same sex, `NA`
  for sexes other than female and male

All three are `NA` for forms without norms and for ages outside the
range the norms cover.

## Details

WG and WS use the 2022 Wordbank norms, the short forms the MB-CDI
short-form norms: Level I for WG short and Level IIA for WS short.

## Examples

``` r
if (FALSE) { # \dontrun{
pb <- peekbank("2026.1")
cdi <- unnest_aux(pb$subjects, "cdi_responses") |>
  add_cdi_percentiles()
} # }
```
