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
#' WG and WS use the 2022 American English norms, the short forms the MB-CDI
#' short-form norms: Level I for WG short and Level IIA for WS short.
#'
#' @param cdi CDI scores, one per row, e.g. from
#'   `unnest_aux(pb$subjects, "cdi_responses")`. Needs `language`,
#'   `instrument_type`, `measure`, `age` and `rawscore`; `sex` is used if
#'   present. The score's age is read from `cdi_age` instead where there is
#'   one, as [nearest_cdi()] creates that name.
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
  # after nearest_cdi(), the score's age is cdi_age and age is the row's
  age_col <- if ("cdi_age" %in% names(cdi)) "cdi_age" else "age"
  require_columns(cdi, c("language", "instrument_type", "measure", age_col,
                         "rawscore"))
  norms <- utils::read.csv(system.file("extdata", "cdi_norms.csv",
                                       package = "peekbankr"))
  tables <- split(norms[c("percentile", "score")],
                  paste(norms$language, norms$instrument_type, norms$measure,
                        norms$sex, norms$age))

  form <- paste(cdi$language, cdi$instrument_type, cdi$measure)
  norm_age <- floor(cdi[[age_col]] + 0.5)
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

#' Add the CDI scores closest in age
#'
#' For each row of `df`, picks the child's CDI score closest in age in each
#' language, form and measure, among those at most `max_age_diff` months away.
#'
#' @param df A table with `subject_id` and `age`, e.g. `pb$administrations`
#' @param cdi CDI scores, one per row, e.g. from
#'   `unnest_aux(pb$subjects, "cdi_responses")`. Needs `subject_id`, `age`,
#'   `language`, `instrument_type` and `measure`.
#' @param max_age_diff The furthest a CDI score may be from the row's age, in
#'   months
#' @param prefer_later Of a score before and one after the row's age that are
#'   equally close, pick the later one instead of the earlier one
#'
#' @return `df` with the columns of `cdi` added, one row per row of `df`,
#'   language, form and measure, in the order of `df`. The score's age is
#'   `cdi_age`. Columns `cdi` shares with `df`, like `dataset_id`, appear once
#'   and have to hold the same values in both for each subject. Rows without a
#'   score close enough are left out.
#' @export
#'
#' @examples
#' \dontrun{
#' pb <- peekbank("2026.1")
#' cdi <- unnest_aux(pb$subjects, "cdi_responses")
#' nearest_cdi(pb$administrations, cdi, max_age_diff = 3)
#' }
nearest_cdi <- function(df, cdi, max_age_diff, prefer_later = FALSE) {
  cdi <- dplyr::rename(cdi, cdi_age = "age")
  # cdi repeats subject columns like dataset_id that df may have too
  shared <- setdiff(intersect(names(df), names(cdi)), "subject_id")

  joined <- df %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::inner_join(cdi, by = "subject_id", suffix = c("", ".cdi"),
                      relationship = "many-to-many")
  for (col in shared) {
    if (!isTRUE(all.equal(joined[[col]], joined[[paste0(col, ".cdi")]],
                          check.attributes = FALSE))) {
      stop("`df` and `cdi` have different values in `", col, "` for the same ",
           "subject. Rename it in one of them.", call. = FALSE)
    }
  }

  joined %>%
    dplyr::select(-dplyr::all_of(paste0(shared, ".cdi", recycle0 = TRUE))) %>%
    dplyr::filter(abs(.data$cdi_age - .data$age) <= max_age_diff) %>%
    # of two equally close scores, slice_min keeps the first
    dplyr::arrange(if (prefer_later) dplyr::desc(.data$cdi_age) else .data$cdi_age) %>%
    dplyr::slice_min(abs(.data$cdi_age - .data$age), with_ties = FALSE,
                     by = c(".row", "language", "instrument_type", "measure")) %>%
    dplyr::arrange(.data$.row) %>%
    dplyr::select(-".row")
}
