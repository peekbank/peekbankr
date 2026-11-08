# An R interface to peekbank

The `peekbankr` package allows you to access data in
[Peekbank](https://peekbank.github.io/peekbank-website/), an open
database of developmental eye-tracking data, from R. Data are retrieved
from the versioned [peekbank dataset on
Redivis](https://redivis.com/datapages/datasets/peekbank); a
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
client pinned to one release hands you its tables as tidy tibbles
without you having to write queries. The package vignette provides
examples of the data loading functions and what the resulting data look
like.

### Installation

``` r

# install.packages("remotes")
remotes::install_github("peekbank/peekbankr")
```

peekbankr follows [semantic versioning](https://semver.org), and each
version is tagged. To keep an analysis on one version of the package,
install it by its tag:

``` r

remotes::install_github("peekbank/peekbankr@v1.1.1")
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

For the timepoint tables, the predicates are translated to remote SQL
queries, so only a subset of predicates is available. If you use a
predicate that cannot be translated to a query, an error message will
let you know. Predicates naming `t_norm` on `pb$subset$aoi_timepoints()`
are the exception: they run in R on the downloaded samples, so they do
not make the download smaller. The smaller tables are filtered in R, so
they take arbitrary `dplyr` expression.

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
#>   methods: clear_cache() images() query() readmes()
#>   subset:  $subset$<table>(<dplyr predicates>)
#>   db:      $db$<table> for a dbplyr handle, read on collect()
```

### Database versions

Peekbank is released in named versions (e.g. `2026.1`), and
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
requires one.
[`peekbank_releases()`](https://peekbank.github.io/peekbankr/reference/peekbank_releases.md)
lists what is available.

Pass `"current"` to take whichever version was published most recently
for testing, but this is strongly discouraged as future version releases
will make the same code produce different results.

``` r

pb <- peekbank("2026.1")
pb$release   # "2026.1"
```

### Keeping downloads between sessions

Whatever a client reads stays cached until the R session ends. With
`cache_on_disk = TRUE` it is also saved in a `.pbcache` folder in the
working directory, so later sessions read it from there instead of
downloading it again:

``` r

pb <- peekbank("2026.1", cache_on_disk = TRUE)
```

### Data files

Each release holds the README and raw data of its datasets, so the
stimulus images and READMEs you get match the release. Releases from
before that take them from the oldest release that holds them:

``` r

readmes <- pb$readmes(datasets = "pomper_saffran_2016")  # dataset documentation
stimuli <- pb$images(datasets = "reflook_v4")             # stimulus images
```

### Development

#### Setup

Clone the repo, then install the development tools and the package’s
dependencies:

``` r

install.packages(c("devtools", "lintr"))
devtools::install_dev_deps()
```

To run the network tests and build the vignette, copy
`.Renviron.template` to `.Renviron`, fill in a personal API token with
the `data.metadata` scope from your [Redivis
workspace](https://redivis.com/workspace) settings, and set
`PEEKBANK_NETWORK_TESTS=true`.

#### The repo

- `R/client.R`: the
  [`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
  client, consumed by users
- `R/redivis.R`, `R/tables.R`, `R/files.R`, `R/cache.R`: contain the
  main functionality powering the client, i.e. reading a release from
  Redivis, shaping its tables, downloading related files and caching
  what was read
- `R/aux_data.R`, `R/cdi.R`: helper functions for aux data and CDI
  scores
- `R/ds_*.R`: the `ds.` functions are used by import script writers,
  contains e.g. validation
- `R/legacy.R`: the superseded interface we keep in the codebase for
  backwards compatibility
- `inst/peekbank-schema.json`: the peekbank schema components like the
  validator use
- `inst/extdata/`: CDI norms and item counts, built by
  `data-raw/build-cdi-reference.R`

#### Repos directly dependent on this one

- [peekbank-data-import](https://github.com/langcog/peekbank-data-import)
  processes each dataset with the help of the `ds.` functions and checks
  it with
  [`ds.validate_for_db_import()`](https://peekbank.github.io/peekbankr/reference/ds.validate_for_db_import.md).
- [peekbank-website](https://github.com/peekbank/peekbank-website)
  documents the exported functions for users. If you change the behavior
  or structure of any exported function, be sure to update that one as
  well.

#### Checklist before pushing

- Run `devtools::document()` after changing roxygen comments, then
  `devtools::test()`,
  [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html)
  and `devtools::check()`. CI runs check, lint, the network tests and
  the pkgdown build on every push (see `.github/workflows/`).
- [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
  checks that every export has a place in `_pkgdown.yml`.
- Right now people install directly from GitHub, so bump `Version` in
  `DESCRIPTION` on every push that changes behavior. Since version
  `1.0.0`, peekbankr follows semantic versioning:
  \<Break\>.\<Add\>.\<Fix\>
- After pushing a new version, tag it so people can install it by its
  tag (see Installation): `git tag vX.Y.Z`, then
  `git push origin vX.Y.Z`.
