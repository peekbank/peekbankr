# Peekbank client

Peekbank client

Peekbank client

## Details

A client pins one Peekbank release, so every table read through it comes
from the same version of the data. Create one with
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md).

Every table is reachable three ways: `pb$administrations` for the whole
table, fetched once per session and reused after that;
`pb$subset$administrations()` to narrow it with `dplyr` predicates; and
`pb$db$administrations` for a `dbplyr` handle that defers transfer until
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Active bindings

- `release`:

  The Peekbank release this client is pinned to

- `redivis_tag`:

  The Redivis version this client is pinned to

- `tables`:

  Names of the tables in this release

- `subset`:

  One function per table taking `dplyr` predicates, e.g.
  `pb$subset$aoi_timepoints(age < 24)`. A predicate can name any column
  of the table or of anything a row of it belongs to: an
  administration's dataset and subject; a trial's trial type, with its
  target and distractor stimuli (prefixed `target_` and `distractor_`)
  and its AOI region set; a timepoint's administration and trial. The
  result has the table's own columns either way. For the timepoint
  tables the predicates run on the server, so only the rows you asked
  for are transferred; the rest are narrowed from the cached table.

- `db`:

  `dbplyr` handles on this release's tables, one field each. They carry
  the same columns as the fields above, but nothing is transferred until
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).
  `aoi_timepoints_rle` is here too, one row per run of a constant AOI,
  for anyone who wants the stored shape rather than the samples it
  expands to.

- `datasets`:

  The datasets table

- `subjects`:

  The subjects table. Subjects link longitudinal administrations;
  `administrations` is usually the one you want.

- `administrations`:

  The administrations table, with `dataset_name` joined on

- `trials`:

  The trials table, with `dataset_id` and `dataset_name` joined on

- `trial_types`:

  The trial types table, with `dataset_name` joined on

- `stimuli`:

  The stimuli table, with `dataset_name` joined on

- `aoi_region_sets`:

  The AOI region sets table

- `aoi_timepoints`:

  Every AOI timepoint in the release: 36 million rows, a little over
  half a gigabyte once expanded. It transfers run-length encoded and is
  expanded locally. `pb$subset$aoi_timepoints()` filters on the server
  instead.

- `xy_timepoints`:

  Every XY timepoint in the release: 10 million rows

## Methods

### Public methods

- [`PeekbankClient$new()`](#method-PeekbankClient-new)

- [`PeekbankClient$query()`](#method-PeekbankClient-query)

- [`PeekbankClient$download_stimuli()`](#method-PeekbankClient-download_stimuli)

- [`PeekbankClient$readmes()`](#method-PeekbankClient-readmes)

- [`PeekbankClient$clear_cache()`](#method-PeekbankClient-clear_cache)

- [`PeekbankClient$print()`](#method-PeekbankClient-print)

------------------------------------------------------------------------

### Method `new()`

Pin a database version.

#### Usage

    PeekbankClient$new(version)

#### Arguments

- `version`:

  A Peekbank release name such as "2025.1", or "current" for whichever
  was published most recently. Required: see
  [`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md).

------------------------------------------------------------------------

### Method `query()`

Run a BigQuery Standard SQL query against this release

#### Usage

    PeekbankClient$query(sql)

#### Arguments

- `sql`:

  A valid SQL query string

------------------------------------------------------------------------

### Method [`download_stimuli()`](https://peekbank.github.io/peekbankr/reference/download_stimuli.md)

Download stimulus images for this release's stimuli

#### Usage

    PeekbankClient$download_stimuli(
      local_base_dir = "stimulus_data",
      datasets = c(),
      skip_existing = TRUE
    )

#### Arguments

- `local_base_dir`:

  Directory to save the images to

- `datasets`:

  Dataset names to download for; all datasets if empty

- `skip_existing`:

  Skip a file that already exists locally at that path

------------------------------------------------------------------------

### Method `readmes()`

Download dataset README files.

#### Usage

    PeekbankClient$readmes(datasets = c(), local_base_dir = "dataset_readmes")

#### Arguments

- `datasets`:

  Dataset names to download for; all datasets if empty

- `local_base_dir`:

  Directory to save the READMEs to

------------------------------------------------------------------------

### Method `clear_cache()`

Drop this version's (instance's) cached tables and query results, so the
next read fetches them again.

#### Usage

    PeekbankClient$clear_cache()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the pinned release and what is reachable on it

#### Usage

    PeekbankClient$print(...)

#### Arguments

- `...`:

  Ignored

## Examples

``` r
if (FALSE) { # \dontrun{
pb <- peekbank("2026.1")

pb$administrations
pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")
pb$db$aoi_timepoints |> dplyr::count(aoi) |> dplyr::collect()
} # }
```
