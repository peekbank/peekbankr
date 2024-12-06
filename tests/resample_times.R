# test file fords.resample_times

# friendly case
df_trial <- tibble(t_norm = c(33, 66, 99, 132, 165), 
                   aoi = c("target","target","missing","distractor","distractor"), 
                   administration_id = 1, 
                   trial_id = 1, 
                   point_of_disambiguation = 0)

resampled <-ds.resample_times(df_trial, table_type = "aoi_timepoints")


# friendly case 2
df_trial <- tibble(t_norm = c(33, 66, 99, 132, 165), 
                   aoi = c("target","target","distractor","missing","distractor"), 
                   administration_id = 1, 
                   trial_id = 1, 
                   point_of_disambiguation = 0)

resampled <-ds.resample_times(df_trial, table_type = "aoi_timepoints")


# what happens in a large gap?
df_trial <- tibble(t_norm = c(33, 66, 99, 1032, 1065), 
                   aoi = c("target","target","distractor","missing","distractor"), 
                   administration_id = 1, 
                   trial_id = 1, 
                   point_of_disambiguation = 0)

resampled <-ds.resample_times(df_trial, table_type = "aoi_timepoints")


# collision - multiple values at a timepoint
df_trial <- tibble(t_norm = c(1, 33, 33, 99), 
                   aoi = c("missing", "target","distractor","distractor"), 
                   administration_id = 1, 
                   trial_id = 1, 
                   point_of_disambiguation = 0)

resampled <-ds.resample_times(df_trial, table_type = "aoi_timepoints")


# time-points that are already at the resampled points
df_trial <- tibble(t_norm = c(33, 66, 99, 100, 132, 165), 
                   aoi = c("target","target","missing","target", "distractor","distractor"), 
                   administration_id = 1, 
                   trial_id = 1, 
                   point_of_disambiguation = 0)

resampled <-ds.resample_times(df_trial, table_type = "aoi_timepoints")
