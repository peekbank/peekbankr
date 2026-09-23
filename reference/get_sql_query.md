# Run a SQL query against peekbank

Queries are BigQuery Standard SQL, run against the peekbank dataset on
Redivis. Note that string comparison is case-sensitive.

## Usage

``` r
get_sql_query(sql_query_string, connection = NULL)
```

## Arguments

- sql_query_string:

  A valid SQL query string

- connection:

  A connection to the peekbank database

## Value

A tibble of the query results

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_peekbank()
get_sql_query("SELECT * FROM datasets", connection = con)
} # }
```
