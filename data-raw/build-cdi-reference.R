# Builds the CDI reference tables shipped in inst/extdata:
#
#   cdi_norms.csv    the raw score at each percentile, by language, instrument,
#                    measure, sex and age in months
#   cdi_n_items.csv  the number of words on each CDI form
#
#   Rscript data-raw/build-cdi-reference.R
#
# Norms come from the 2022 Wordbank benchmarks for English WG and WS
# (data-raw/cdi-benchmarks-2022) and the English short-form norms Virginia
# Marchman shared in peekbank/peekbankr#19 (data-raw/cdi-norms-shortform).
# Level I is the WG short form. Peekbank does not record which of the two
# parallel Level II forms a lab used, so the WS short form always uses Level IIA.
#
# needs REDIVIS_API_TOKEN in .Renviron to run

library(dplyr)

# one norms file: a row of ages, then one row of scores per percentile
read_norms <- function(path, instrument_type, measure, sex) {
  sheet <- utils::read.csv(path, row.names = 1)
  is_age_row <- rownames(sheet) == "age"

  scores <- sheet[!is_age_row, , drop = FALSE]
  names(scores) <- as.numeric(sheet[is_age_row, ])
  scores$percentile <- as.numeric(rownames(scores))

  scores %>%
    tidyr::pivot_longer(!"percentile", names_to = "age", values_to = "score",
                        names_transform = as.numeric) %>%
    mutate(
      language = "English (American)",
      instrument_type = .env$instrument_type,
      measure = .env$measure,
      sex = .env$sex
    ) %>%
    select("language", "instrument_type", "measure", "sex",
           "age", "percentile", "score")
}

benchmarks <- "data-raw/cdi-benchmarks-2022"
shortform <- "data-raw/cdi-norms-shortform"
sexes <- c(both = "both", f = "female", m = "male")
shortform_sexes <- c(both = "both", girls = "female", boys = "male")

norms <- bind_rows(
  lapply(names(sexes), function(s) bind_rows(
    read_norms(file.path(benchmarks, paste0("eng_wg_comp_", s, ".csv")), "wg", "comp", sexes[[s]]),
    read_norms(file.path(benchmarks, paste0("eng_wg_prod_", s, ".csv")), "wg", "prod", sexes[[s]]),
    read_norms(file.path(benchmarks, paste0("eng_ws_prod_", s, ".csv")), "ws", "prod", sexes[[s]])
  )),
  lapply(names(shortform_sexes), function(s) bind_rows(
    read_norms(file.path(shortform, paste0("LevelI_comp_", s, "_weighted.csv")), "wgshort", "comp", shortform_sexes[[s]]),
    read_norms(file.path(shortform, paste0("LevelI_prod_", s, "_weighted.csv")), "wgshort", "prod", shortform_sexes[[s]]),
    read_norms(file.path(shortform, paste0("LevelIIA_prod_", s, "_weighted.csv")), "wsshort", "prod", shortform_sexes[[s]])
  ))
) %>%
  arrange(.data$instrument_type, .data$measure, .data$sex, .data$age, .data$percentile)

utils::write.csv(norms, "inst/extdata/cdi_norms.csv", row.names = FALSE)

forms <- tibble(
  form            = c("WG", "WS", "WG Short", "WS Short"),
  instrument_type = c("wg", "ws", "wgshort", "wsshort")
)

item_counts <- redivis::redivis$organization("datapages")$dataset("wordbank:627v")$query("
  SELECT language, form, COUNT(*) AS n_items
  FROM items:1mzm
  WHERE item_kind = 'word' AND form IN ('WG', 'WS', 'WG Short', 'WS Short')
  GROUP BY language, form
")$to_tibble() %>%
  inner_join(forms, by = "form") %>%
  select("language", "instrument_type", "n_items") %>%
  arrange(.data$language, .data$instrument_type)

utils::write.csv(item_counts, "inst/extdata/cdi_n_items.csv", row.names = FALSE)
cat("wrote", nrow(norms), "norm rows and", nrow(item_counts), "forms\n")
