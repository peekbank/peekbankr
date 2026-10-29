#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang ":="
NULL

# expand a run-length-encoded AOI timepoints table (one row per run of a
# constant AOI within an administration x trial, with t_norm the run start
# and `length` the run length in samples) back to one row per 25 ms sample.
# Columns named in `carry` are constant within a run and repeat along with it.
decode_rle_timepoints <- function(aoi_timepoints, carry = character()) {
  timestep <- 1000 / pkg_globals$SAMPLE_RATE
  runs <- as.integer(aoi_timepoints$length)
  # one element per output sample: the run it came from, and how far into it
  source_row <- rep.int(seq_len(nrow(aoi_timepoints)), runs)
  offset <- sequence(runs) - 1L

  decoded <- dplyr::tibble(
    administration_id = aoi_timepoints$administration_id[source_row],
    trial_id = aoi_timepoints$trial_id[source_row],
    aoi = aoi_timepoints$aoi[source_row],
    t_norm = as.integer(aoi_timepoints$t_norm[source_row] + offset * timestep)
  )
  for (col in carry) decoded[[col]] <- aoi_timepoints[[col]][source_row]
  decoded
}


#' Expand one key of a table's aux data into rows
#'
#' Reads one key from the JSON in a table's `*_aux_data` column. A key holding
#' a list of records, like `cdi_responses`, gives one row per record; a key
#' holding a single value gives one row with that value in a column named
#' after the key. Rows without the key are dropped. The table's other columns
#' are kept.
#'
#' @param df A Peekbank table with a `*_aux_data` column, e.g. `pb$subjects`
#' @param key The aux key to expand, e.g. `"cdi_responses"`
#'
#' @return A tibble with the table's columns, without the aux column, followed
#'   by the fields of `key`
#' @export
#'
#' @examples
#' \dontrun{
#' pb <- peekbank("2026.1")
#' cdi <- unnest_aux(pb$subjects, "cdi_responses")
#' exposures <- unnest_aux(pb$subjects, "lang_exposures")
#' }
unnest_aux <- function(df, key) {
  aux_col <- grep("_aux_data$", names(df), value = TRUE)
  if (length(aux_col) != 1) {
    stop("`df` needs exactly one *_aux_data column", call. = FALSE)
  }
  table_cols <- dplyr::as_tibble(df)[setdiff(names(df), aux_col)]

  row_entries <- lapply(df[[aux_col]], function(aux_json) {
    value <- if (is.na(aux_json)) NULL else jsonlite::fromJSON(aux_json)[[key]]
    if (length(value) == 0) return(NULL)
    if (is.list(value)) dplyr::as_tibble(value) else dplyr::tibble(!!key := value)
  })
  n_entries <- vapply(row_entries,
                      function(entry) if (is.null(entry)) 0L else nrow(entry),
                      integer(1))
  aux_fields <- dplyr::bind_rows(row_entries)

  name_clashes <- intersect(names(aux_fields), names(table_cols))
  if (length(name_clashes) > 0) {
    warning("Dropped aux fields that repeat a column of the table: ",
            paste(name_clashes, collapse = ", "), call. = FALSE)
    aux_fields <- aux_fields[setdiff(names(aux_fields), name_clashes)]
  }
  # a row that produced no entries gets zero copies, so it drops out
  dplyr::bind_cols(tidyr::uncount(table_cols, .env$n_entries), aux_fields)
}
