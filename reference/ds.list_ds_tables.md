# List the tables required based on coding method

List the tables required based on coding method

## Usage

``` r
ds.list_ds_tables(coding_methods = c("eyetracking"))
```

## Arguments

- coding_methods:

  a list of strings indicating the methods used in the experiment for
  coding gaze data, to get the list of current coding methods, please
  use ds.list_coding_methods()

## Value

a list of table types that are required based on input coding method

## Examples

``` r
if (FALSE) { # \dontrun{
table_list <- ds.list_ds_tables(coding_methods = "manual gaze coding")
} # }
```
