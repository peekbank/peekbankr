# Expand one key of a table's aux data into rows

Reads one key from the JSON in a table's `*_aux_data` column. A key
holding a list of records, like `cdi_responses`, gives one row per
record; a key holding a single value gives one row with that value in a
column named after the key. Rows without the key are dropped. The
table's other columns are kept.

## Usage

``` r
unnest_aux(df, key)
```

## Arguments

- df:

  A Peekbank table with a `*_aux_data` column, e.g. `pb$subjects`

- key:

  The aux key to expand, e.g. `"cdi_responses"`

## Value

A tibble with the table's columns, without the aux column, followed by
the fields of `key`

## Examples

``` r
if (FALSE) { # \dontrun{
pb <- peekbank("2026.1")
cdi <- unnest_aux(pb$subjects, "cdi_responses")
exposures <- unnest_aux(pb$subjects, "lang_exposures")
} # }
```
