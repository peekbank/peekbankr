<!-- badges: start -->
[![R-CMD-check](https://github.com/langcog/peekbankr/workflows/R-CMD-check/badge.svg)](https://github.com/langcog/peekbankr/actions)
<!-- badges: end -->

# An R interface to peekbank

The `peekbankr` package allows you to access data in peekbank from R. This removes the need to write complex SQL queries in order to get the information you want from the database. The package vignette provides some examples of how to use the data loading functions and what the resulting data look like.

### Install `peekbankr` from GitHub:

```
install.packages("RMariaDB")
# install.packages("remotes")
remotes::install_github("peekbank/peekbankr")
```

### Local Installation from Source

When developing, you can run:

```
# install.packages("RMariaDB")
install.packages(".", repos = NULL, type="source", dependencies=TRUE)
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

con <- connect_to_peekbank()

aoi_timepoints <- get_aoi_timepoints(connection = con, dataset_name = "pomper_saffran_2016")
administrations <- get_administrations(connection = con, dataset_name = "pomper_saffran_2016")

ps_data <- aoi_timepoints %>%
  left_join(administrations)
```

### TLS / SSL

`connect_to_peekbank()` handles TLS automatically via the `ssl` argument (default `"auto"`):

* The hosted Peekbank instance is contacted over TLS and verified against a CA cert shipped inside the package; nothing to configure.
* Connections to `127.0.0.1` or `localhost` skip TLS enforcement, so a local `peekbank` docker stack running without TLS works out of the box.
* For any other host, TLS handling is left to the connector defaults.

To override:

```
# Custom self-hosted server with its own self-signed cert:
connect_to_peekbank(host = "db.example.org", ssl = "/path/to/my-ca.pem")

# Force plaintext (e.g. an unusual non-localhost development setup):
connect_to_peekbank(host = "192.168.1.42", ssl = "disabled")
```
