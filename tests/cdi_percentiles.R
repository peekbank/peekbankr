# This test requires a database connection and should only be run manually
if (identical(Sys.getenv("PEEKBANK_TEST_DB"), "true")) {

library(peekbankr)
library(dplyr)
library(tidyr)

con <- connect_to_peekbank(db_version = "grant_2024_dev")
all_aoi_timepoints <- get_aoi_timepoints(connection = con, rle=FALSE)

# reload connection in case it is stale
con <- connect_to_peekbank(db_version = "grant_2024_dev")
all_stimuli <- collect(get_stimuli(connection = con))
all_administrations <- collect(get_administrations(connection = con))
all_subjects <- unpack_aux_data(collect(get_subjects(connection = con)))
all_trial_types <- collect(get_trial_types(connection = con))
all_trials <- unpack_aux_data(collect(get_trials(connection = con)))
all_datasets <- get_datasets(connection=con) %>% collect()

cdi_data <- all_subjects %>%
  unnest(subject_aux_data) %>%
  filter(!is.na(cdi_responses)) %>%
  unnest(cdi_responses) %>%
  peekbankr::cleanup_cdi_data() %>%
  peekbankr::append_relative_cdi_scores() %>%
  peekbankr::populate_cdi_percentiles()

}
