# Connect to Peekbank

Returns a client pinned to one Peekbank release. Every table read
through it comes from that version, so an analysis cannot mix releases.

## Usage

``` r
peekbank(version, cache_on_disk = FALSE)
```

## Arguments

- version:

  A Peekbank release name such as "2025.1". Pass "current" to take
  whichever version was published most recently, or "next" for the
  unreleased draft of the next release (only editors of the dataset on
  Redivis can read it)

- cache_on_disk:

  Also keep what is read in a `.pbcache` folder in the working
  directory, so that later sessions read it from there instead of
  downloading it again. They still need to reach Redivis, to resolve the
  release and to build timepoint queries. Not available for "next",
  whose contents change.

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
