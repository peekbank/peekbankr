# Check if a certain table is required according to schema

Check if a certain table is required according to schema

## Usage

``` r
ds.is_table_required(table_type, coding_methods)
```

## Arguments

- table_type:

  the type of dataframe, for the most updated table types specified by
  schema, please use ds.list_ds_tables()

- coding_methods:

  methods used in the experiment for coding gaze data, to get the list
  of current coding methods, please use function
  ds.list_coding_methods()

## Value

A boolean value

## Examples

``` r
if (FALSE) { # \dontrun{
is_required <- ds.is_table_required(table_type = "xy_timepoints",
                                 coding_method = "manual gaze coding")
} # }
```
