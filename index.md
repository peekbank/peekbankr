# An R interface to peekbank

The `peekbankr` package allows you to access data in
[Peekbank](https://peekbank.github.io/peekbank-website/), an open
database of developmental eye-tracking data, from R. Data are retrieved
from the versioned [peekbank dataset on
Redivis](https://redivis.com/datapages/datasets/peekbank); the `get_`
functions return tidy tables without you having to write queries. The
package vignette provides examples of the data loading functions and
what the resulting data look like.

### Installation

``` r

# install.packages("remotes")
remotes::install_github("peekbank/peekbankr")
```

The first data request will open a browser window to authorize Redivis
access (free account). For non-interactive use, set a
`REDIVIS_API_TOKEN` environment variable instead. That token needs the
`data.metadata` scope.

### Usage

``` r

library(tidyverse)
library(peekbankr)

pb <- peekbank("2026.1")   # create a peekbank client with a database version

data <- pb$aoi_timepoints %>%
  left_join(pb$administrations)
```

This downloads the datasets automatically for the first access and
caches them for subsequent use.

If you only want to download a subset of the release,
`pb$subset$<table>()` takes `dplyr` expressions and returns only the
matching rows:

``` r

aoi_timepoints <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")
administrations <- pb$subset$administrations(dataset_name == "pomper_saffran_2016")

ps_data <- aoi_timepoints %>%
  left_join(administrations)
```

Predicates are `dplyr` expressions. A predicate can name any column of
the table, or columns of any table that a row of the queried table is
connected to:

``` r

pb$subset$aoi_timepoints(age >= 24, age <= 36, sex == "female")
pb$subset$aoi_timepoints(vanilla_trial, !excluded)
pb$subset$aoi_timepoints(target_stimulus_novelty == "familiar")
pb$subset$administrations(sex == "female")
pb$subset$trials(vanilla_trial)
```

These also work across tables:

``` r

pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016",
                         age < 24 & vanilla_trial)
```

The exception is **`t_norm`** on `pb$subset$aoi_timepoints()`, as by
default we transfers the table run-length encoded (one row per run of a
constant AOI) and t_norm filters would result in unintuitive behavior.
If you need to filter t_norm, either do it on the fetched result OR pass
`rle = FALSE` to read the expanded table directly (which will make the
fetch take a lot longer):

``` r

pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016", t_norm > 0) # Error

pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016", t_norm > 0,
                         rle = FALSE)
```

Every other column filters normally, including `aoi`, which is constant
within a run. `pb$subset$xy_timepoints()` is not encoded, so it has no
such restriction.

For the timepoint tables, the predicates are translated to remote SQL
queries, so only a subset of predicates is available. If you use a
predicate that cannot be translated to a query, an error message will
let you know. The smaller tables are filtered in R, so they take
arbitrary `dplyr` expression.

### Server-side queries

`pb$db` holds a [dbplyr](https://dbplyr.tidyverse.org/) handle on each
table. `dplyr` verbs on it compile to SQL and run on the server; data
transfer happens when you `collect()`. Here is an example that
summarises a timepoint table without pulling it:

``` r

pb$db$aoi_timepoints %>%
  group_by(aoi) %>%
  count() %>%
  collect()
```

`pb$db` also holds `aoi_timepoints_rle`, one row per run of a constant
AOI, for anyone who wants the stored shape rather than the samples it
expands to.

Print the client to see the release it is pinned to and what is on it:

``` r

pb
#> <peekbank> release 2026.1 (Redivis version v1.4)
#>   tables:  datasets subjects administrations trials trial_types stimuli
#>            aoi_region_sets aoi_timepoints xy_timepoints
#>   methods: clear_cache() download_stimuli() query() readmes()
#>   subset:  $subset$<table>(<dplyr predicates>)
#>   db:      $db$<table> for a dbplyr handle, read on collect()
```

### Database versions

Peekbank is released in named versions (e.g. `2026.1`), and
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
requires one.
[`get_db_info()`](https://peekbank.github.io/peekbankr/reference/get_db_info.md)
lists what is available.

Pass `"current"` to take whichever version was published most recently
for testing, but this is strongly discouraged as future version releases
will make the same code produce different results.

``` r

pb <- peekbank("2026.1")
pb$release   # "2026.1"
```

### Data files

Raw data, processed intermediates, and per-dataset READMEs live in the
companion [peekbank_files
dataset](https://redivis.com/datapages/datasets/peekbank_files):

``` r

pb$readmes(datasets = "pomper_saffran_2016")       # dataset documentation
stimuli <- pb$download_stimuli(datasets = "reflook_v4")  # stimulus images
```

### Development

After making changes, run `roxygen2::roxygenise()` to update exports and
documentation, and
[`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
for the offline test suite.

Running the full test suite requires Redivis credentials: Without them
the network tests skip. Copy `.Renviron.template` to `.Renviron`, fill
in a personal API token from your [Redivis
workspace](https://redivis.com/workspace) settings, and set
`PEEKBANK_NETWORK_TESTS=true`.
