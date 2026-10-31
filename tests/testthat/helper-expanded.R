# aoi_timepoints read from the expanded table, the reference the decoded
# run-length encoding is checked against
expanded_aoi_timepoints <- function(pb, ...) {
  pb$.__enclos_env__$private$timepoints("aoi_timepoints", rlang::enquos(...))
}
