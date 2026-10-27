# Accessing Peekbank Data

## Overview

The `peekbankr` package allows you to access data in the peekbank
database from R. Data are hosted in the versioned [peekbank dataset on
Redivis](https://redivis.com/datapages/datasets/peekbank); the client
retrieves tidy tables from it without you having to write queries. This
vignette shows some examples of how to use the data loading functions
and what the resulting data look like.

Every table in the release is a field on the client, holding the whole
table:

- `pb$datasets`
- `pb$subjects`
- `pb$administrations`
- `pb$trials`
- `pb$trial_types`
- `pb$stimuli`
- `pb$aoi_region_sets`
- `pb$aoi_timepoints`
- `pb$xy_timepoints`

To work with part of a release, `pb$subset$<table>()` takes `dplyr`
expressions. For the timepoint tables, which are far too large to narrow
locally, these run on the server.

**Technical note 1**: The first request will open a browser window to
authorize Redivis access (or set a `REDIVIS_API_TOKEN`).

**Technical note 2**: Start by creating a client with
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md).
It pins a database version for your whole analysis, and every table you
read from it comes from that same version. The version is required,
because Peekbank is re-released over time and an unpinned script quietly
reads different data as those releases land. Pass `"current"` if you do
want whichever version is newest, which is not always the highest
release number. The older
[`connect_to_peekbank()`](https://peekbank.github.io/peekbankr/reference/connect_to_peekbank.md)
/ `get_*(connection = con)` style still works.

**Technical note 3**: We have tried to optimize the time it takes to get
data from the database (the AOI timepoints transfer run-length-encoded
and are expanded locally). But if you query entire timepoint tables, it
will still take a while, as you are transferring 100s of MB of data.

``` r

# load the libraries
library(peekbankr)
library(dplyr)

# pin a database version for all the calls below
pb <- peekbank("2026.1")
```

## Get datasets

The `datasets` field holds a table related to the sources of the
dataset, information of the tracker, information of the method (e.g.,
monitor size and sample rate).

For example, reading `pb$datasets` gives you every dataset in the
database.

``` r

d_datasets <- pb$datasets
head(d_datasets)
```

## Get Subjects

The `subjects` field holds information about persistent subject
identifiers for noting when subjects have participated in multiple
experiments. This includes demographic information (currently only sex
and lab-specific subject id).

``` r

d_subjects <- pb$subjects
head(d_subjects)
```

## Auxiliary data

Several tables carry a `*_aux_data` column holding whatever a lab
recorded that the schema has no place for, as a JSON string. In practice
that is mostly `subjects`, with the keys `cdi_responses`,
`lang_measures`, `lang_exposures` and `native_language_non_iso`.
[`unnest_aux()`](https://peekbank.github.io/peekbankr/reference/unnest_aux.md)
turns one key into rows, one per entry, keeping the subject’s other
columns:

``` r

cdi <- unnest_aux(pb$subjects, "cdi_responses")
```

[`add_cdi_percentiles()`](https://peekbank.github.io/peekbankr/reference/add_cdi_percentiles.md)
adds where each score falls in the vocabulary norms, and
[`add_cdi_proportions()`](https://peekbank.github.io/peekbankr/reference/add_cdi_proportions.md)
the share of the form’s words:

``` r

cdi <- unnest_aux(pb$subjects, "cdi_responses") %>%
  add_cdi_percentiles() %>%
  add_cdi_proportions()
```

Predicates can reach the column before it is unpacked, but only as text,
since the server sees a JSON string:

``` r

pb$subset$aoi_timepoints(grepl("cdi_responses", subject_aux_data))
```

## Get Administrations

The `administrations` field holds information about the specific
experimental administrations to subjects in the database. This includes
information about:

- age
- monitor size
- tracker

Again, if you run the function with no arguments, then you get all the
information for all administrations in the database, but you can now
also filter on a dataset name or dataset id.

``` r

d_administrations <- pb$subset$administrations(dataset_name == "pomper_saffran_2016")
head(d_administrations)
```

The age argument takes a number indicating the age(s) of children (in
months) that you want to analyze. you can use this argument in two ways

1.  Pass a single number to get information about all participants who
    were tested at that particular age.
2.  Pass a range of ages to get information about all participants who
    were tested within a certain age range.

For example, you can get the participant information for all of the
children who were tested between the ages of 24 and 36 months.

``` r

d_age_range <- pb$subset$administrations(age >= 24, age <= 36)
head(d_age_range)
```

## Get trials

The `trials` field holds a table with information of the trials in the
experiments in the database. This includes the following information:

- Phrase
- Language
- Point of disambiguation
- IDs to link to other tables.

``` r

d_trials <- pb$trials
head(d_trials)
```

This function also takes dataset name and id filters.

## Get stimuli

The `stimuli` field holds a table with information of the stimuli in the
experiments in the database. This includes the following information:

- Label
- Image
- Novelty status

``` r

d_stimuli <- pb$stimuli
head(d_stimuli)
```

This function also takes dataset name and id filters.

## Get AOI region sets

The `aoi_region_sets` field holds a table with the information of the
region of area of interest (AOI) for experiments using eye-trackers. It
includes information of the dimensions of the x and y, such as the
minimum and maximum dimension of the xy spaces.

``` r

d_aoi_region_sets <- pb$aoi_region_sets
head(d_aoi_region_sets)
```

This function is not expected to be used commonly - this information is
retained as part of the process of calculating AOIs from XY points.

## Get AOI timepoints

The `aoi_timepoints` field holds a table with information of the
subject’s looking behavior in each trial. For example, you can get
information about which area that the subject was looking at in a
particular trial (e.g., looking away or target or distractor).

Narrow it with `pb$subset$aoi_timepoints()`, which takes `dplyr`
expressions that run on the server. As everywhere in `pb$subset`, an
expression can name any column of the table or of anything a row of it
belongs to. A timepoint belongs to an administration — with its dataset
and subject (`dataset_name == "pomper_saffran_2016"`, `age < 24`,
`sex == "female"`) — and to a trial, with its trial type and its two
stimuli (`vanilla_trial`, `!excluded`,
`target_stimulus_novelty == "familiar"`). These are expressions rather
than named arguments, so `==` and not `=`.

The `t_norm` field provides a trial-normalized time variable
(milliseconds) whose 0 point is the point of disambiguation on that
trial (first timestep of the onset of the first time the target label is
said).

``` r

d_aoi_timepoints <- pb$subset$aoi_timepoints(dataset_name == "pomper_saffran_2016")
head(d_aoi_timepoints)
```

## Get XY timepoints

For experiments using eye-trackers (as opposed to hand coding from
video), the `xy_timepoints` field holds a table including the x and y
position across time.

``` r

d_xy_timepoints <- pb$subset$xy_timepoints(dataset_name == "reflook_v4")
```

## Query without downloading

`pb$db` holds a [dbplyr](https://dbplyr.tidyverse.org/) handle on each
table. `dplyr` verbs on these compile to SQL and run on the server, and
nothing is transferred until you
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html). The
handles carry the same columns as the fields above, so a summary over a
timepoint table costs you four rows rather than 36 million.

``` r

pb$db$aoi_timepoints %>%
  count(aoi) %>%
  collect()
```

`pb$db` also holds `aoi_timepoints_rle`, the run-length encoded form the
`aoi_timepoints` field expands locally: one row per run of a constant
AOI, with `length` the number of 25 ms samples in the run.
