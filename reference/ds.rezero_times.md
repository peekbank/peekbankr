# Shift each trial's times to start at zero

Shift each trial's times to start at zero

## Usage

``` r
ds.rezero_times(df_table)
```

## Arguments

- df_table:

  Timepoints with `administration_id`, `trial_id` and `t`, the lab's
  timestamps in ms

## Value

`df_table` with `t` replaced by `t_zeroed`, the time since the trial's
first timestamp
