# Peekbank releases

The releases
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
can be pinned to, newest Redivis version first.

## Usage

``` r
peekbank_releases()
```

## Value

A tibble of one row per release, with the Peekbank `release` name, the
`redivis_tag` of the version holding it, and whether it is `current`

## Details

The `current` release is the one `peekbank("current")` resolves to: the
most recently published Redivis version. That is not always the highest
release number, because an older release is sometimes republished.

## Examples

``` r
if (FALSE) { # \dontrun{
peekbank_releases()
} # }
```
