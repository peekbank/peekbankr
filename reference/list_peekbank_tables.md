# List of peekbank tables

Superseded. Use `pb$tables` on a
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
client.

## Usage

``` r
list_peekbank_tables(connection)
```

## Arguments

- connection:

  A connection to the peekbank database

## Value

A vector of the names of tables in peekbank

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_peekbank()
list_peekbank_tables(con)
} # }
```
