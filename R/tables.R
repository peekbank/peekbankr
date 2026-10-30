# The tables in the dhape peekbankr hands out, derived from the ones redivis.R pulled.

new_connection <- function(version) {
  ver <- resolve_release(version)
  structure(
    list(release = ver$release, redivis_tag = ver$redivis_tag),
    class = "peekbank_connection"
  )
}

fetch_table_names <- function(con) {
  names <- pb_try(
    vapply(pb_dataset(con$redivis_tag)$list_tables(), function(t) {
      if (!is.null(t$name)) t$name else t$properties$name
    }, character(1)))
  if (is.null(names)) return(NULL)
  sort(unname(names))
}

# the tables whose rows belong to a dataset, and so can be given its name
dataset_joined_tables <- c("administrations", "subjects", "trials",
                           "trial_types", "stimuli")

# One of Redivis's tables turned into one peekbankr hands out:
# * `dataset_name` joined on where the table references a dataset
# * the 0/1 flags as logicals, unless `logical_flags` is FALSE
# * `aoi_timepoints` put in the shape its encoded form decodes to
#
# `read_table` supplies one of Redivis's tables by name so the same joins
# can run on tibbles for pb$x and on dbplyr handles for pb$db$x
table_view <- function(name, read_table, logical_flags = TRUE) {
  if (identical(name, "aoi_timepoints")) {
    # the columns and the order decode_rle_timepoints() produces, so the two
    # encodings are interchangeable.
    return(dplyr::select(read_table(name),
                         dplyr::all_of(c("administration_id", "trial_id",
                                         "aoi", "t_norm"))))
  }
  tbl <- read_table(name)
  if (logical_flags) tbl <- as_logical_flags(name, tbl)
  if (!name %in% dataset_joined_tables) return(tbl)
  if (identical(name, "subjects")) {
    # a subject is tested in one dataset, reached through its administrations
    tbl <- dplyr::left_join(
      tbl,
      dplyr::distinct(dplyr::select(
        read_table("administrations"),
        dplyr::all_of(c("subject_id", "dataset_id")))),
      by = "subject_id")
  }
  if (identical(name, "trials")) {
    # trials reach their dataset through trial_types
    tbl <- dplyr::left_join(
      tbl,
      dplyr::select(read_table("trial_types"),
                    dplyr::all_of(c("trial_type_id", "dataset_id"))),
      by = "trial_type_id")
  }
  dplyr::inner_join(
    tbl,
    dplyr::select(read_table("datasets"),
                  dplyr::all_of(c("dataset_id", "dataset_name"))),
    by = "dataset_id")
}

# The schema stores these two as 0/1 integers, turn them into logicals
# Releases up to 2022.1 predate the two relevant columns, hence the checks.
as_logical_flags <- function(name, tbl) {
  cols <- colnames(tbl)
  if (identical(name, "trials") && "excluded" %in% cols) {
    tbl <- dplyr::mutate(tbl, excluded = .data$excluded == 1)
  }
  if (identical(name, "trial_types") && "vanilla_trial" %in% cols) {
    tbl <- dplyr::mutate(tbl, vanilla_trial = .data$vanilla_trial == 1)
  }
  tbl
}

# the view read into memory, by passing the reader that returns tibbles
fetch_table <- function(con, name, logical_flags = TRUE) {
  table_view(name, function(n) pb_table(con, n), logical_flags)
}

# join `other` on, keeping only the columns `frame` does not already have
# peekbank does not have overlapping columns, but our intermediate
# representations and views have
widen_with <- function(frame, other, by) {
  extra <- setdiff(colnames(other), colnames(frame))
  dplyr::left_join(frame, dplyr::select(other, dplyr::all_of(c(by, extra))),
                   by = by)
}

# special handleing for trial types, as they points at two different stimuli
# (and at an AOI region set)
add_trial_type_links <- function(frame, read_table) {
  stimulus <- function(prefix) {
    read_table("stimuli") %>%
      dplyr::select(-dplyr::all_of("dataset_id")) %>%
      dplyr::rename_with(function(n) paste0(prefix, n))
  }
  frame %>%
    dplyr::left_join(stimulus("target_"),
                     by = c("target_id" = "target_stimulus_id")) %>%
    dplyr::left_join(stimulus("distractor_"),
                     by = c("distractor_id" = "distractor_stimulus_id")) %>%
    dplyr::left_join(read_table("aoi_region_sets"), by = "aoi_region_set_id")
}

# What a predicate may name when narrowing a table: the table itself plus
# everything that can be uniquely matched to one of its rows.
# Every join is many-to-one.
predicate_view <- function(name, read_table) {
  frame <- table_view(name, read_table)
  switch(name,
    administrations = frame %>%
      widen_with(read_table("subjects"), "subject_id") %>%
      widen_with(read_table("datasets"), "dataset_id"),
    trials = frame %>%
      widen_with(add_trial_type_links(table_view("trial_types", read_table),
                                      read_table), "trial_type_id") %>%
      widen_with(read_table("datasets"), "dataset_id"),
    trial_types = widen_with(add_trial_type_links(frame, read_table),
                             read_table("datasets"), "dataset_id"),
    subjects = ,
    stimuli = widen_with(frame, read_table("datasets"), "dataset_id"),
    # a timepoint belongs to an administration and to a trial
    aoi_timepoints = ,
    aoi_timepoints_rle = ,
    xy_timepoints = frame %>%
      widen_with(predicate_view("administrations", read_table),
                 "administration_id") %>%
      widen_with(predicate_view("trials", read_table), "trial_id"),
    # anything else stands alone: it belongs to nothing
    frame)
}
