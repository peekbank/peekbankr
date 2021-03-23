<!-- badges: start -->
[![R-CMD-check](https://github.com/langcog/peekbankr/workflows/R-CMD-check/badge.svg)](https://github.com/langcog/peekbankr/actions)
<!-- badges: end -->

# An R interface to peekbank

The `peekbankr` package allows you to access data in peekbank from R. This removes the need to write complex SQL queries in order to get the information you want from the database. The package vignette provides some examples of how to use the data loading functions and what the resulting data look like.

### Install `peekbankr` from GitHub:

```
# install.packages("remotes")
remotes::install_github("langcog/peekbankr")
```

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

### Other relevant GitHub repositories

- Peekbank data import scripts: http://github.com/langcog/peekbank-data-import
- Peek data standard and data import functions: http://github.com/langcog/peekds
- Peekbank database: http://github.com/langcog/peekbank
- Interactive data visualizations using shiny: http://github.com/langcog/peekbank-shiny
- Website frontend: http://github.com/langcog/peekbank-website
