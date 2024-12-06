<!-- badges: start -->
[![R-CMD-check](https://github.com/langcog/peekbankr/workflows/R-CMD-check/badge.svg)](https://github.com/langcog/peekbankr/actions)
<!-- badges: end -->

# An R interface to peekbank

The `peekbankr` package allows you to access data in peekbank from R. This removes the need to write complex SQL queries in order to get the information you want from the database. The package vignette provides some examples of how to use the data loading functions and what the resulting data look like.

### Install `peekbankr` from GitHub:

```
# install.packages("remotes")
remotes::install_github("peekbank/peekbankr")
```

### Local Installation from Source

When developing, you can run:

```
install.packages(".", repos = NULL, type="source", dependencies=TRUE)
```

If it fails to install the `RMariaDB` dependency automatically, you can manually trigger the installation using

```
install.packages("RMariaDB")
```

After making changes, be sure to run 

```
roxygen2::roxygenise()
```

to update exports and documentation.

### Usage

Here's a simple workflow for using `peekbankr` to get data from a single study. 

```
library(tidyverse)
library(peekbankr)

aoi_timepoints <- get_aoi_timepoints(dataset_name = "pomper_saffran_2016")
administrations <- get_administrations(dataset_name = "pomper_saffran_2016")

ps_data <- aoi_timepoints %>%
  left_join(administrations)
```

### Data Codebook

For an overview of the individual columns in each dataset, see the following codebook:
https://docs.google.com/spreadsheets/d/e/2PACX-1vR4AiOkIzIMbb2C9ksCpu6aWqYaIEiA72voek4y_05y9eY9J6XS5tLhnHZ5xnDk9LxKihicd0gN9BZY/pubhtml

### Other relevant GitHub repositories

- Peekbank data import scripts: http://github.com/peekbank/peekbank-data-import
- Peek data standard and data import functions: http://github.com/peekbank/peekds
- Peekbank database: http://github.com/peekbank/peekbank
- Interactive data visualizations using shiny: http://github.com/peekbank/peekbank-shiny
- Website frontend: http://github.com/peekbank/peekbank-website
