#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang ":="
NULL

# expand a run-length-encoded AOI timepoints table (one row per run of a
# constant AOI within an administration x trial, with t_norm the run start
# and `length` the run length in samples) back to one row per 25 ms sample.
decode_rle_timepoints <- function(aoi_timepoints) {
  timestep <- 1000 / pkg_globals$SAMPLE_RATE
  runs <- as.integer(aoi_timepoints$length)
  # one element per output sample: the run it came from, and how far into it
  source_row <- rep.int(seq_len(nrow(aoi_timepoints)), runs)
  offset <- sequence(runs) - 1L

  dplyr::tibble(
    administration_id = aoi_timepoints$administration_id[source_row],
    trial_id = aoi_timepoints$trial_id[source_row],
    aoi = aoi_timepoints$aoi[source_row],
    t_norm = as.integer(aoi_timepoints$t_norm[source_row] + offset * timestep)
  )
}


#' Unpack the json sting in the *_aux_data column and turns
#' it into a nested R list
#'
#'
#' @param df a dataframe in the peekbank format that has an aux data column
#'
#' @return the input dataframe, with the *_aux_data column unpacked
#'
#' @export
#'
#' @examples
#' \dontrun{
#' subjects_table <- unpack_aux_data(df = subjects_table)
#' }
unpack_aux_data <- function(df) {
  aux_names <- colnames(df)[stringr::str_which(colnames(df), ".*_aux_data$")]
  for (aux_name in aux_names) {
    df <- unpack_one_aux(df, aux_name)
  }
  df
}

# one *_aux_data column, from json string to a nested column of its fields
unpack_one_aux <- function(df, aux_name) {
  aux_list <- df |>
    dplyr::ungroup() |>
    dplyr::pull(dplyr::all_of(aux_name)) |>
    lapply(\(aux) {
      if (is.null(aux) || is.na(aux)) {
        return(aux)
      }
      jsonlite::fromJSON(aux)
    })
  if (all(is.na(aux_list))) {
    return(df)
  }

  col_names <- purrr::flatten(aux_list) |>
    names() |>
    unique()
  col_names <- col_names[!is.na(col_names) & col_names != ""]

  aux_cols <- lapply(col_names, \(col_name) {
    sapply(aux_list, \(aux) {
      # cursed way to make sure that there are no "NULL" strings left over
      # from weird jsonlite::fromJSON behavior,
      # check https://github.com/jeroen/jsonlite/issues/70 to see if there has been a fix by now
      if (length(aux) == 1 &&
          (is.na(aux) ||
           is.null(aux[col_name]) ||
           aux[col_name] == "NULL"
          ) || (
            all(is.na(aux)) ||
            all(is.null(aux[col_name])) ||
            all(aux[col_name] == "NULL")
          )
      ) {
        return(NA)
      }
      aux[col_name]
    })
  }) |>
    `names<-`(value = col_names) |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(aux) {
      if (any(sapply(aux, \(aux_val) {
        typeof(aux_val) == "list"
      }))) {
        aux <- lapply(aux, \(aux_val) {
          if (all(is.na(aux_val))) {
            return(NA)
          }
          dplyr::bind_rows(aux_val)
        })
      }
      if (all(sapply(aux, is.atomic))) {
        aux <- purrr::list_simplify(aux, strict = FALSE) # May need a better fix for NAs
      }
      aux
    }))
  df |>
    cbind(aux_cols) |>
    dplyr::select(-dplyr::all_of(aux_name)) |>
    tidyr::nest("{aux_name}" := dplyr::all_of(colnames(aux_cols)))
}
