<!-- badges: start -->
[![R-CMD-check](https://github.com/peekbank/peekbankr/actions/workflows/check.yml/badge.svg)](https://github.com/peekbank/peekbankr/actions)
<!-- badges: end -->

# An R interface to peekbank

The `peekbankr` package allows you to access data in
[Peekbank](https://peekbank.github.io/peekbank-website/), an open database of
developmental eye-tracking data, from R. Data are retrieved from the
versioned [peekbank dataset on Redivis](https://redivis.com/datapages/datasets/peekbank);
the `get_` functions return tidy tables without you having to write queries.
The package vignette provides examples of the data loading functions and
what the resulting data look like.

### Installation

`peekbankr` uses the `redivis` client (not on CRAN) to access the data:

```r
install.packages("redivis", repos = "https://langcog.r-universe.dev", type = "source")
# install.packages("remotes")
remotes::install_github("peekbank/peekbankr")
```

The first data request will open a browser window to authorize Redivis
access (free account). For non-interactive use, set a `REDIVIS_API_TOKEN`
environment variable instead.

### Usage

Here's a simple workflow for using `peekbankr` to get data from a single study.

```r
library(tidyverse)
library(peekbankr)

con <- connect_to_peekbank()   # pins the current database version

aoi_timepoints <- get_aoi_timepoints(connection = con, dataset_name = "pomper_saffran_2016")
administrations <- get_administrations(connection = con, dataset_name = "pomper_saffran_2016")

ps_data <- aoi_timepoints %>%
  left_join(administrations)
```

### Database versions

Peekbank is released in named versions (e.g. `2026.1`). By default
`connect_to_peekbank()` uses the latest release and prints which one that
is; pass `db_version = "2025.1"` (or a Redivis version tag like `"v1.2"`)
to pin an earlier release for reproducibility. `get_db_info()` lists the
available versions.

### Data files

Raw data, processed intermediates, and per-dataset READMEs live in the
companion [peekbank_files dataset](https://redivis.com/datapages/datasets/peekbank_files):

```r
get_readmes(datasets = "pomper_saffran_2016")          # dataset documentation
stimuli <- download_stimuli(con, datasets = "reflook_v4")  # stimulus images
```

### Development

After making changes, run `roxygen2::roxygenise()` to update exports and
documentation, and `testthat::test_local()` for the offline test suite. Two
gated suites need environment variables: `PEEKBANK_NETWORK_TESTS=true`
(live Redivis reads) and `PEEKBANK_FIXTURES_DIR=<path>` (characterization
against the MySQL-era fixtures; see
[peekbank-datapage](https://github.com/peekbank/peekbank-datapage)
`migration/capture_fixtures.R`).
