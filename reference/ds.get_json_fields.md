# Fetching the list of field names and requirements in each table according to the schema json file

Fetching the list of field names and requirements in each table
according to the schema json file

## Usage

``` r
ds.get_json_fields(table_type)
```

## Arguments

- table_type:

  the type of dataframe, for the most updated table types specified by
  schema, please use ds.list_ds_tables()

## Value

the list of field names

## Examples

``` r
if (FALSE) { # \dontrun{
fields_json <- ds.get_json_fields(table_type = "aoi_timepoints")
} # }
```
