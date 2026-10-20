# Get information on database connection options

Superseded. Use
[`peekbank_releases()`](https://peekbank.github.io/peekbankr/reference/peekbank_releases.md).

## Usage

``` r
get_db_info()
```

## Value

List of database info: `current` (the Peekbank release that is the
latest Redivis version) and `versions` (a named vector mapping Peekbank
release names to Redivis dataset version tags)

## Details

Data are retrieved from the versioned peekbank dataset on Redivis.
Version information is discovered from the `release_info` table naming
the Peekbank release it holds.

## Examples

``` r
if (FALSE) { # \dontrun{
get_db_info()
} # }
```
