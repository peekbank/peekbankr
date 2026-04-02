# returns a logical vector indicating which resampled points fall inside
# large gaps (>3x median interval) between consecutive original timepoints
find_gap_points <- function(t_origin, t_resampled) {
  if (length(t_origin) < 2) return(rep(FALSE, length(t_resampled)))
  gap_threshold <- 3 * stats::median(diff(t_origin))
  original_gaps <- diff(t_origin)
  interval_idx <- findInterval(t_resampled, t_origin)
  # clamp to valid index range so original_gaps[safe_idx] doesn't drop elements
  safe_idx <- pmax(pmin(interval_idx, length(original_gaps)), 1)
  interval_idx > 0 & interval_idx < length(t_origin) &
    original_gaps[safe_idx] > gap_threshold
}

# key private function to do resampling of aois within a single trial uses
# approxfun to resample because missingness is coded as an integer and
# interpolation is "constant" then no "gaps" between AOIs are filled. actually
# interpolating across blinks is left for a different function as this is a
# theory-laden decision.
resample_aoi_trial <- function(df_trial) {
  t_origin <- df_trial$t_norm
  data_origin <- df_trial$aoi

  # create the new timestamps for resampling
  t_start <- min(t_origin) - (min(t_origin) %% pkg_globals$SAMPLE_DURATION)
  t_resampled <- seq(from = t_start, to = max(t_origin),
                     by = pkg_globals$SAMPLE_DURATION)

  # exchange strings values with integers for resampling
  # this step critical for interpolating missing vals quickly and correctly
  aoi_num <- data_origin %>%
    dplyr::recode(target = 1, distractor = 2, other = 3, missing = 4)

  # start resampling with approx
  aoi_resampled <- stats::approx(x = t_origin, y = aoi_num, xout = t_resampled,
                                 method = "constant", rule = 2,
                                 ties = "ordered")$y
  aoi_resampled[find_gap_points(t_origin, t_resampled)] <- 4  # missing

  aoi_resampled_recoded <- aoi_resampled %>%
    dplyr::recode("1" = "target", "2" = "distractor",
                  "3" = "other", "4" = "missing")

  # adding back the columns to match schema
  dplyr::tibble(t_norm = t_resampled,
                aoi = aoi_resampled_recoded,
                trial_id = df_trial$trial_id[1],
                administration_id = df_trial$administration_id[1])
}

# key private function to do resampling of aois within a single trial
# uses approxfun to resample
resample_xy_trial <- function(df_trial) {
  MISSING_CONST <- -10000

  t_origin <- df_trial$t_norm
  x_origin <- df_trial$x
  y_origin <- df_trial$y

  # create the new timestamps for resampling
  t_start <- min(t_origin) - (min(t_origin) %% pkg_globals$SAMPLE_DURATION)
  t_resampled <- seq(from = t_start, to = max(t_origin),
                     by = pkg_globals$SAMPLE_DURATION)

  # because of the behavior of approx, we need numerical values for missingness
  x_origin[is.na(x_origin)] <- MISSING_CONST
  y_origin[is.na(y_origin)] <- MISSING_CONST

  # resample we use constant interpolation for two reasons: 1) the numerical
  # missingness needs to be constant, if you interpolate it you won't be able to
  # back it out, 2) linear interpolation might "slow down" saccades by choosing
  # intermediate locations (minor).
  x_resampled <- stats::approx(x = t_origin, y = x_origin, xout = t_resampled,
                               method = "constant", rule = 2,
                               ties = "ordered")$y
  y_resampled <- stats::approx(x = t_origin, y = y_origin, xout = t_resampled,
                               method = "constant", rule = 2,
                               ties = "ordered")$y

  in_gap <- find_gap_points(t_origin, t_resampled)
  x_resampled[in_gap] <- MISSING_CONST
  y_resampled[in_gap] <- MISSING_CONST

  # replace missing values
  x_resampled[x_resampled == MISSING_CONST] <- NA
  y_resampled[y_resampled == MISSING_CONST] <- NA

  # adding back the columns to match schema
  # note, no IDs here because they won't be unique.
  dplyr::tibble(trial_id = df_trial$trial_id[1],
                administration_id = df_trial$administration_id[1],
                t_norm = t_resampled,
                x = x_resampled,
                y = y_resampled)
}

#' sets the starting point of a given trial to be zero
#'
#' @param df_table to-be-resampled dataframe with t, aoi/xy values, trial_id and
#'   administration_id
#'
#' @return df_out with resampled time, xy or aoi value rows

#' @export
ds.rezero_times <- function(df_table) {
  # first check if this data frame has all the correct columns required for
  # normalize
  required_columns <- c("trial_id", "administration_id", "t")

  if (!all(required_columns %in% colnames(df_table))) {
    stop(.msg("Rezero times function requires the following columns to be
              present in the dataframe:
              {paste(required_columns, collapse = ', ')}. Rezeroing times should
              be the first step in the time standardization process."))
  }
  # center timestamp (0 POD)
  df_out <- df_table %>%
    dplyr::arrange(.data$trial_id, .data$t) %>%
    dplyr::group_by(.data$administration_id, .data$trial_id) %>%
    dplyr::mutate(t_zeroed = (.data$t - .data$t[1])) %>%
    dplyr::select(-.data$t)
  return(df_out)
}

#' sets the starting point of a given trial to be zero
#'
#' @param df_table to-be-resampled dataframe with t, aoi/xy values, trial_id and
#'   administration_id
#'
#' @return df_out with resampled time, xy or aoi value rows
#'
#' @export
ds.normalize_times <- function(df_table) {
  # first check if this data frame has all the correct columns required for
  # normalize
  required_columns <- c("trial_id", "administration_id", "t_zeroed")

  if (!all(required_columns %in% colnames(df_table))) {
    stop(.msg("Normalize times function requires the following columns to be
              present in the dataframe:
              {paste(required_columns, collapse = ', ')}. Times should be
              re-zeroed first to the starting point of a given trial before
              being normalized."))
  }
  # center timestamp (0 POD)
  df_out <- df_table %>%
    dplyr::group_by(.data$administration_id, .data$trial_id) %>%
    dplyr::mutate(t_norm = .data$t_zeroed - .data$point_of_disambiguation) %>%
    dplyr::select(-.data$t_zeroed)
  return(df_out)
}

# nothing necessarily needs to be changed but we could either arrange the
# resulting dataset by trial_id and administration_id or else use a different
# workflow, for example nest after grouping by administration_id and trial_id?

#' This function resample times to be consistent across labs.
#'
#' Resampling is done by the following steps:
#'
#' 1. iterate through every trial for every administration
#'
#' 2. create desired timepoint sequence with equal spacing according to
#' pre-specified SAMPLE_RATE parameter
#'
#' 3. use approxfun to interpolate given data points to align with desired
#' timepoint sequence "constant" interpolation method is used for AOI
#' timepoints; "linear" interpolation method is used for xy timepoints; for more
#' details on approxfun, please see:
#' https://stat.ethz.ch/R-manual/R-devel/library/stats/html/approxfun.html
#'
#' 4. after resampling, bind resampled dataframes back together and re-assign
#' aoi_timepoint_id
#'
#' @param df_table to-be-resampled dataframe with t, aoi/xy values, trial_id and
#'   administration_id
#'
#' @param table_type table name, can only be "aoi_timepoints" or "xy_timepoints"
#'
#' @return df_out with resampled time, xy or aoi value rows
#'
#'
#' @examples
#'
#' \dontrun{
#' dir_datasets <- "testdataset" # local datasets dir
#' lab_dataset_id <- "pomper_saffran_2016"
#' dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")
#' table_type <- "aoi_timepoints"
#' file_csv <- file.path(dir_csv, paste0(table_type, '.csv'))
#' df_table <- utils::read.csv(file_csv)
#' df_resampled <-ds.resample_times(df_table, table_type = "aoi_timepoints")
#' }
#'
#' @export
ds.resample_times <- function(df_table, table_type) {

  # first check if this data frame has all the correct columns required for
  # re-sampling
  if (table_type == "aoi_timepoints") {
    required_columns <- c("trial_id", "administration_id", "t_norm", "aoi")
  } else if (table_type == "xy_timepoints") {
    required_columns <- c("trial_id", "administration_id", "t_norm", "x", "y")
  }

  # re-zero and normalize times first
  # this is mandatory, comes from our decision that not linking resampling and
  # centering causes a lot of problems
  if (!all(required_columns %in% colnames(df_table))) {
    stop(.msg("Resample times function requires the following columns to be
              present in the dataframe:
              {paste(required_columns, collapse = ', ')}. Times should be
              re-zeroed and normalized first before being resampled!"))
  }

  # check that t_norm is monotonically increasing within each trial
  non_monotonic <- df_table %>%
    dplyr::group_by(.data$administration_id, .data$trial_id) %>%
    dplyr::filter(dplyr::lag(.data$t_norm, default = -.Machine$integer.max) >= .data$t_norm) %>%
    dplyr::ungroup()

  if (nrow(non_monotonic) > 0) {
    affected <- non_monotonic %>%
      dplyr::distinct(.data$administration_id, .data$trial_id)
    stop(.msg("t_norm values are not monotonically increasing within {nrow(affected)} trial(s). This may indicate timestamp resets within a trial. Affected administration_id/trial_id pairs: {paste(affected$administration_id, affected$trial_id, sep = '/', collapse = ', ')}"))
  }

  # main resampling call
  if (table_type == "aoi_timepoints") {
    # start resampling process by iterating through every trial within every
    # administration
    df_out <- df_table %>%
      dplyr::mutate(admin_trial_id = paste(.data$administration_id,
                                           .data$trial_id, sep = "_")) %>%
      split(.$admin_trial_id) %>%
      purrr::map_df(resample_aoi_trial) %>%
      dplyr::arrange(.data$administration_id, .data$trial_id) %>%
      dplyr::mutate(aoi_timepoint_id = 0:(dplyr::n() - 1)) # add IDs
  } else if (table_type == "xy_timepoints") {
    df_out <- df_table %>%
      dplyr::mutate(admin_trial_id = paste(.data$administration_id,
                                           .data$trial_id, sep = "_")) %>%
      split(.$admin_trial_id) %>%
      purrr::map_df(resample_xy_trial) %>%
      dplyr::arrange(.data$administration_id, .data$trial_id) %>%
      dplyr::mutate(xy_timepoint_id = 0:(dplyr::n() - 1)) # add IDs
  }

  return(df_out)
}

#' Add AOIs to an xy dataframe
#'
#' @param xy_joined dataframe containing processed xy timepoints with aoi region
#'   sets information
#'
#' @return dataframe with two added columns 'side' and 'aoi'. 'side' only
#'   contains "left" or "right" value 'aoi' indicates whether this xy timepoint
#'   is looking to "target" or "distractor"
#' @export
ds.add_aois <- function(xy_joined) {
  xy_joined %<>%
    dplyr:: mutate(
      side = dplyr::case_when(
        x > l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "left",
        x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "right",
        !is.na(x) & !is.na(y) ~ "other",
        TRUE ~ "missing"),
      aoi = dplyr::case_when(
        side %in% c("left", "right") & side == target_side ~ "target",
        side %in% c("left", "right") & side != target_side ~ "distractor",
        TRUE ~ side # other or NA, which is same as side
      ))

  return(xy_joined)
}
