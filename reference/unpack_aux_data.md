# Unpack the json sting in the \*\_aux_data column and turns it into a nested R list

Unpack the json sting in the \*\_aux_data column and turns it into a
nested R list

## Usage

``` r
unpack_aux_data(df)
```

## Arguments

- df:

  a dataframe in the peekbank format that has an aux data column

## Value

the input dataframe, with the \*\_aux_data column unpacked

## Examples

``` r
if (FALSE) { # \dontrun{
subjects_table <- unpack_aux_data(df = subjects_table)
} # }
```
