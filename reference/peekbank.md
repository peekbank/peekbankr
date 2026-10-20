# Connect to Peekbank

Returns a client pinned to one Peekbank release. Every table read
through it comes from that version, so an analysis cannot mix releases.

## Usage

``` r
peekbank(version)
```

## Arguments

- version:

  A Peekbank release name such as "2025.1". Pass "current" to take
  whichever version was published most recently, which is not always the
  highest release number.

## Value

A
[PeekbankClient](https://peekbank.github.io/peekbankr/reference/PeekbankClient.md)

## Details

The version has no default. Peekbank is re-released over time, and a
script that does not name a release reads different data as those
releases land, silently and without a diff. Naming one is the difference
between an analysis that can be re-run and one that cannot.

## Examples

``` r
if (FALSE) { # \dontrun{
pb <- peekbank("2026.1")
pb

# whole tables
administrations <- pb$administrations

# narrowed, on the server for the timepoint tables
aoi <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")

# computed on the server, nothing transferred until collect()
pb$db$aoi_timepoints |> dplyr::count(aoi) |> dplyr::collect()
} # }
```
