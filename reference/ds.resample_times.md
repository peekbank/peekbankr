# This function resample times to be consistent across labs.

Resampling is done by the following steps:

## Usage

``` r
ds.resample_times(df_table, table_type)
```

## Arguments

- df_table:

  to-be-resampled dataframe with t_norm, aoi/xy values, trial_id and
  administration_id

- table_type:

  table name, can only be "aoi_timepoints" or "xy_timepoints"

## Value

df_out with resampled time, xy or aoi value rows

## Details

1.  iterate through every trial for every administration

2.  create desired timepoint sequence with equal spacing according to
    pre-specified SAMPLE_RATE parameter

3.  use [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) with
    "constant" interpolation, for AOI and xy timepoints alike, to give
    each new timepoint the value of the last sample at or before it

4.  after resampling, bind resampled dataframes back together and
    re-assign aoi_timepoint_id or xy_timepoint_id

## Examples

``` r

if (FALSE) { # \dontrun{
dir_datasets <- "testdataset" # local datasets dir
lab_dataset_id <- "pomper_saffran_2016"
dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")
table_type <- "aoi_timepoints"
file_csv <- file.path(dir_csv, paste0(table_type, '.csv'))
df_table <- utils::read.csv(file_csv)
df_resampled <- ds.resample_times(df_table, table_type = "aoi_timepoints")
} # }
```
