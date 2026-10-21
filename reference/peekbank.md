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
  whichever version was published most recently

## Value

A
[PeekbankClient](https://peekbank.github.io/peekbankr/reference/PeekbankClient.md)

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
