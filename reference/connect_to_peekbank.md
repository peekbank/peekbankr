# Connect to Peekbank

As of peekbankr 0.4, data are retrieved from the versioned peekbank
dataset on Redivis rather than a MySQL server. \`connect_to_peekbank()\`
now returns a lightweight handle that pins a database version (Redivis
dataset version); it no longer opens a DBI connection. Existing code
that passes the result via the \`connection\` argument of the \`get\_\`
functions continues to work.

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

  String of the name of database version to use: "current" (the default;
  resolves to the latest Redivis version), a Peekbank release name (e.g.
  "2025.1"), or a Redivis version tag (e.g. "v1.2")

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

A \`peekbank_connection\` handle pinning the resolved version

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_peekbank(db_version = "current")
} # }
```
