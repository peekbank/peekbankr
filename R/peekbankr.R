#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang ":="
NULL

# expand a run-length-encoded AOI timepoints table (one row per run of a
# constant AOI within an administration x trial, with t_norm the run start
# and `length` the run length in samples) back to one row per 25 ms sample.
# Rows must be ordered by t_norm within each administration x trial.
decode_rle_timepoints <- function(aoi_timepoints) {
  timestep <- 1000 / pkg_globals$SAMPLE_RATE

  aoi_timepoints %>%
    tidyr::nest(trial_data = -dplyr::all_of(c("administration_id", "trial_id"))) %>%
    dplyr::mutate(
      rle_vector = purrr::map(.data$trial_data, function(td) {
        `class<-`(list(lengths = as.integer(td$length), values = td$aoi), "rle")
      }),
      aoi = purrr::map(.data$rle_vector, inverse.rle),
      t_norm = purrr::map(.data$trial_data, function(td) {
        as.integer(seq(
          td$t_norm[1], td$t_norm[1] + (sum(td$length) - 1) * timestep,
          timestep
        ))
      })
    ) %>%
    dplyr::select(-dplyr::all_of(c("trial_data", "rle_vector"))) %>%
    tidyr::unnest(cols = dplyr::all_of(c("aoi", "t_norm")))
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
