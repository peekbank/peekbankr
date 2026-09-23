# Classify gaze samples into AOIs based x/y coordinates and provided region sets

Classify gaze samples into AOIs based x/y coordinates and provided
region sets

## Usage

``` r
ds.compute_aois(data)
```

## Arguments

- data:

  a data frame carrying x, y, target_side, the eight region bounds
  (l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max,
  r_y_min) and monitor_size_x / monitor_size_y

## Value

`data` with the `aoi` column set, overwriting any existing value
