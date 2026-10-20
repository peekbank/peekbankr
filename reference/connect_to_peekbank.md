# Connect to Peekbank

Superseded. Use
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md),
which returns a client that pins a release for a whole analysis.

## Usage

``` r
connect_to_peekbank(
  db_version = "current",
  db_args = NULL,
  compress = TRUE,
  host = NULL,
  port = NULL,
  ssl = "auto"
)
```

## Arguments

- db_version:

  String naming the database version to use: a Peekbank release name
  (e.g. "2025.1"), or "current" (the default) for whichever version was
  published most recently

- db_args:

  Deprecated, ignored (retained for backwards compatibility)

- compress:

  Deprecated, ignored

- host:

  Deprecated, ignored

- port:

  Deprecated, ignored

- ssl:

  Deprecated, ignored

## Value

A `peekbank_connection` handle pinning the resolved version

## Details

`connect_to_peekbank()` returns a handle that pins a database version
(Redivis dataset version)

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_peekbank(db_version = "current")
} # }
```
