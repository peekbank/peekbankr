# An R interface to peekbank

The `peekbankr` package allows you to access data in peekbank from R. This removes the need to write complex SQL queries in order to get the information you want from the database. The package vignette provides some examples of how to use the data loading functions and what the resulting data look like.

### Install `peekbankr` from this GitHub repository:

```
# install.packages("devtools")
devtools::install_github("langcog/peekbankr")
```

### Tutorial

Here's a simple workflow for using `peekbankr` to get data from a single study. 

```
library(tidyverse)
library(peekbankr)

aoi_timepoints <- get_aoi_timepoints(dataset_name = "pomper_saffran_2016")
administrations <- get_administrations(dataset_name = "pomper_saffran_2016")
trials <- get_trials(dataset_name = "pomper_saffran_2016")
stimuli <- get_stimuli(dataset_name = "pomper_saffran_2016")

ps_data <- aoi_timepoints %>%
  left_join(administrations) %>%
  left_join(trials) %>%
  left_join(stimuli, by = c("target_id" = "stimulus_id")) 
```

### Other relevant GitHub repositories

- Website frontend: [http://github.com/langcog/peekbank-website]()
- Interactive data visualizations using shiny: [http://github.com/langcog/peekbank-shiny]()
- Peek data standard - for importing data: [http://github.com/langcog/peekds]()
- Peekbank database: [http://github.com/langcog/peekbank]()
