require_columns <- function(df, columns) {
  missing <- setdiff(columns, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

#' Add vocabulary norm percentiles to CDI scores
#'
#' Looks up where each raw score falls in the vocabulary norms for its
#' language, instrument, measure and age in months (rounded to the nearest
#' month). The norms cover English (American):
#' * WG and WG short form, comprehension and production, 8 to 18 months
#' * WS and WS short form, production, 16 to 30 months
#'
#' WG and WS use the 2022 Wordbank norms, the short forms the MB-CDI
#' short-form norms: Level I for WG short and Level IIA for WS short.
#'
#' @param cdi CDI scores, one per row, e.g. from
#'   `unnest_aux(pb$subjects, "cdi_responses")`. Needs `language`,
#'   `instrument_type`, `measure`, `age` and `rawscore`; `sex` is used if
#'   present.
#'
#' @return `cdi` with three columns added:
#' * `norm_age`: the age in months whose norms were used
#' * `norm_percentile`: the highest norm percentile (5, 10, ..., 95, 99) the
#'   score reaches, or 1 below the 5th
#' * `norm_percentile_sex`: the same against children of the same sex, `NA`
#'   for sexes other than female and male
#'
#' All three are `NA` for forms without norms and for ages outside the range
#' the norms cover.
#' @export
#'
#' @examples
#' \dontrun{
#' pb <- peekbank("2026.1")
#' cdi <- unnest_aux(pb$subjects, "cdi_responses") |>
#'   add_cdi_percentiles()
#' }
add_cdi_percentiles <- function(cdi) {
  require_columns(cdi, c("language", "instrument_type", "measure", "age",
                         "rawscore"))
  norms <- utils::read.csv(system.file("extdata", "cdi_norms.csv",
                                       package = "peekbankr"))
  tables <- split(norms[c("percentile", "score")],
                  paste(norms$language, norms$instrument_type, norms$measure,
                        norms$sex, norms$age))

  form <- paste(cdi$language, cdi$instrument_type, cdi$measure)
  norm_age <- floor(cdi$age + 0.5)
  percentile_against <- function(sex) {
    keys <- paste(form, sex, norm_age)
    vapply(seq_along(keys), function(i) {
      table <- tables[[keys[i]]]
      if (is.null(table) || is.na(cdi$rawscore[i])) return(NA_real_)
      max(c(1, table$percentile[table$score <= cdi$rawscore[i]]))
    }, numeric(1))
  }

  normed <- paste(form, "both", norm_age) %in% names(tables)
  cdi$norm_age <- ifelse(normed, norm_age, NA_real_)
  cdi$norm_percentile <- percentile_against("both")
  cdi$norm_percentile_sex <- if ("sex" %in% names(cdi)) {
    percentile_against(cdi$sex)
  } else {
    NA_real_
  }

  no_norms <- !form %in% paste(norms$language, norms$instrument_type,
                               norms$measure)
  out_of_age <- !no_norms & !normed
  if (any(no_norms | out_of_age)) {
    message(sum(no_norms | out_of_age), " of ", nrow(cdi), " scores have no ",
            "norm percentile: ", sum(no_norms), " on forms without norms, ",
            sum(out_of_age), " at ages outside the norms. See ",
            "?add_cdi_percentiles for what the norms cover.")
  }
  cdi
}

#' Add the relative share of a CDI form's words to CDI scores
#'
#' Divides each raw score by the number of words on its form, taken from the
#' form's item list in Wordbank. Comprehension and production on the same form
#' share one word list.
#'
#' @param cdi CDI scores, one per row, e.g. from
#'   `unnest_aux(pb$subjects, "cdi_responses")`. Needs `language`,
#'   `instrument_type` and `rawscore`.
#'
#' @return `cdi` with `n_items`, the number of words on the form, and
#'   `proportion`, the raw score divided by it
#' @export
#'
#' @examples
#' \dontrun{
#' pb <- peekbank("2026.1")
#' cdi <- unnest_aux(pb$subjects, "cdi_responses") |>
#'   add_cdi_proportions()
#' }
add_cdi_proportions <- function(cdi) {
  require_columns(cdi, c("language", "instrument_type", "rawscore"))
  item_counts <- utils::read.csv(system.file("extdata", "cdi_n_items.csv",
                                             package = "peekbankr"))

  form <- paste(cdi$language, cdi$instrument_type)
  known_forms <- paste(item_counts$language, item_counts$instrument_type)
  cdi$n_items <- item_counts$n_items[match(form, known_forms)]
  cdi$proportion <- cdi$rawscore / cdi$n_items

  unknown <- unique(form[is.na(cdi$n_items)])
  if (length(unknown) > 0) {
    message(sum(is.na(cdi$n_items)), " of ", nrow(cdi), " scores are on forms ",
            "without a Wordbank item list: ", paste(unknown, collapse = ", "))
  }
  cdi
}
