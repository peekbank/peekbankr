#' Classify gaze samples into AOIs based x/y coordinates and provided region sets
#'
#' @param data a data frame carrying x, y, target_side, the eight region bounds
#'   (l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, r_y_min) and
#'   monitor_size_x / monitor_size_y
#'
#' @return \code{data} with the \code{aoi} column set, overwriting any existing value
#'
#' @export
ds.compute_aois <- function(data) {
  required <- c("x", "y", "target_side",
                "l_x_max", "l_x_min", "l_y_max", "l_y_min",
                "r_x_max", "r_x_min", "r_y_max", "r_y_min",
                "monitor_size_x", "monitor_size_y")
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "ds.compute_aois() is missing required column(s): ",
      paste(missing_cols, collapse = ", "), ".\n"
    ))
  }
  n_na_monitor <- sum(is.na(data$monitor_size_x) | is.na(data$monitor_size_y))
  if (n_na_monitor > 0) {
    stop(paste(
      "ds.compute_aois(): monitor_size_x/monitor_size_y is NA for", n_na_monitor,
      "of", nrow(data), "samples. The measurements are needed to determine offscreen looks."
    ))
  }

  side <- dplyr::case_when(
    tolower(as.character(data$target_side)) %in% c("left", "l") ~ "left",
    tolower(as.character(data$target_side)) %in% c("right", "r") ~ "right",
    .default = NA_character_
  )

  data %>%
    dplyr::mutate(aoi = dplyr::case_when(
      is.na(.env$side) ~ "missing",
      .env$side == "left" ~ dplyr::case_when(
        x <= l_x_max & x >= l_x_min & y <= l_y_max & y >= l_y_min ~ "target",
        x <= r_x_max & x >= r_x_min & y <= r_y_max & y >= r_y_min ~ "distractor",
        is.na(x) | is.na(y) | x > monitor_size_x | x < 0 | y > monitor_size_y | y < 0 ~ "missing",
        .default = "other"
      ),
      .default = dplyr::case_when(
        x <= l_x_max & x >= l_x_min & y <= l_y_max & y >= l_y_min ~ "distractor",
        x <= r_x_max & x >= r_x_min & y <= r_y_max & y >= r_y_min ~ "target",
        is.na(x) | is.na(y) | x > monitor_size_x | x < 0 | y > monitor_size_y | y < 0 ~ "missing",
        .default = "other"
      )
    ))
}
