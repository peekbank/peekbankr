# Center each trial's times on its point of disambiguation

Center each trial's times on its point of disambiguation

## Usage

``` r
ds.normalize_times(df_table)
```

## Arguments

- df_table:

  The output of
  [`ds.rezero_times()`](https://peekbank.github.io/peekbankr/reference/ds.rezero_times.md),
  with `point_of_disambiguation` in ms from the trial's start

## Value

`df_table` with `t_zeroed` replaced by `t_norm`, the time from the point
of disambiguation
