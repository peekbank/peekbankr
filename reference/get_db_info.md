# Get information on database connection options

As of peekbankr 0.4, data are retrieved from the versioned peekbank
dataset on Redivis (<https://redivis.com/datapages/datasets/peekbank>)
rather than a MySQL server. Version information is discovered from the
Redivis dataset itself: each Redivis version carries a \`release_info\`
table naming the Peekbank release it holds.

## Usage

``` r
get_db_info()
```

## Value

List of database info: \`current\` (the Peekbank release that is the
latest Redivis version) and \`versions\` (a named vector mapping
Peekbank release names to Redivis dataset version tags)

## Examples

``` r
if (FALSE) { # \dontrun{
get_db_info()
} # }
```
