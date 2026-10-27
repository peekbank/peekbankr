# Add the relative share of a CDI form's words to CDI scores

Divides each raw score by the number of words on its form, taken from
the form's item list in Wordbank. Comprehension and production on the
same form share one word list.

## Usage

``` r
add_cdi_proportions(cdi)
```

## Arguments

- cdi:

  CDI scores, one per row, e.g. from
  `unnest_aux(pb$subjects, "cdi_responses")`. Needs `language`,
  `instrument_type` and `rawscore`.

## Value

`cdi` with `n_items`, the number of words on the form, and `proportion`,
the raw score divided by it

## Examples

``` r
if (FALSE) { # \dontrun{
pb <- peekbank("2026.1")
cdi <- unnest_aux(pb$subjects, "cdi_responses") |>
  add_cdi_proportions()
} # }
```
