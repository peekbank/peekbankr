# Function for mapping raw data columns to processed table columns

Function for mapping raw data columns to processed table columns

## Usage

``` r
ds.map_columns(raw_data, raw_format, table_type)
```

## Arguments

- raw_data:

  raw data frame

- raw_format:

  source of the eye-tracking data, e.g. "tobii"

- table_type:

  type of processed table, e.g. "xy_data" \| "aoi_table"

## Value

processed data frame with specified column names

## Examples

``` r
if (FALSE) { # \dontrun{
df_xy_data <- ds.map_columns(raw_data = raw_data, raw_format = "tobii",
                          table_type = "xy_data")
df_aoi_data <- ds.map_columns(raw_data = raw_data, raw_format = "tobii",
                           table_type = "aoi_data")
} # }
```
