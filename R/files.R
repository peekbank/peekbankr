# stimulus images for a release, saved under dir; returns the stimuli table
# with the local path of each image
fetch_stimulus_images <- function(con, datasets = c(), dir = "stimulus_data",
                                  skip_existing = TRUE) {
  stimuli_df <- fetch_table(con, "stimuli") %>%
    dplyr::filter(!is.na(.data$stimulus_image_path))

  if (length(datasets) > 0) {
    stimuli_df <- stimuli_df %>%
      dplyr::filter(.data$dataset_name %in% .env$datasets)
  }

  wanted <- stimuli_df %>%
    dplyr::mutate(full_stimulus_path = paste0(.data$dataset_name, "/raw_data/",
                                              .data$stimulus_image_path))

  # files live in the peekbank_files dataset on Redivis; one query per
  # dataset keeps the file listing small
  listings <- lapply(unique(wanted$dataset_name), function(ds) {
    pb_files_query(paste0(ds, "/raw_data/"))
  })
  index <- dplyr::bind_rows(listings)
  files <- dplyr::inner_join(
    wanted %>% dplyr::distinct(.data$full_stimulus_path),
    index, by = c("full_stimulus_path" = "file_name")) %>%
    dplyr::rename(file_name = "full_stimulus_path")

  missing <- setdiff(wanted$full_stimulus_path, files$file_name)
  if (length(missing) > 0) {
    message(length(missing), " stimulus files were not found in the ",
            "peekbank_files dataset (e.g. ", missing[1], ")")
  }

  local <- pb_download_files(files, dir, skip_existing = skip_existing)
  path_map <- stats::setNames(local, files$file_name)
  message("Downloaded ", sum(!is.na(local)), " stimulus files from Redivis")

  return(wanted %>%
    dplyr::mutate(local_image_path =
                    unname(path_map[.data$full_stimulus_path])) %>%
    dplyr::select(-"full_stimulus_path"))
}

# the READMEs of a release's datasets, saved as <dataset>.md under dir;
# returns the datasets table with the local path and text of each README
fetch_readmes <- function(con, datasets = c(), dir = "dataset_readmes") {
  datasets_df <- fetch_table(con, "datasets")
  if (length(datasets) > 0) {
    datasets_df <- datasets_df %>%
      dplyr::filter(.data$dataset_name %in% .env$datasets)
  }

  paths <- download_readmes(datasets_df$dataset_name, dir)
  if (is.null(paths)) return(NULL)
  local <- unname(paths[datasets_df$dataset_name])

  datasets_df %>%
    dplyr::mutate(
      local_readme_path = local,
      readme = vapply(local, function(path) {
        if (is.na(path)) return(NA_character_)
        paste(readLines(path, encoding = "UTF-8", warn = FALSE),
              collapse = "\n")
      }, character(1), USE.NAMES = FALSE)
    )
}

# the READMEs of the named datasets (every one in peekbank_files if NULL),
# saved as <dataset>.md under dir; returns their local paths named by dataset,
# NA where a download failed
download_readmes <- function(datasets, dir) {
  # READMEs live at <dataset>/README.md in the peekbank_files dataset
  index <- pb_files_query("")
  if (is.null(index)) return(NULL)
  readmes <- index[grepl("^[^/]+/README\\.md$", index$file_name), ]
  ds_names <- sub("/README\\.md$", "", readmes$file_name)
  if (!is.null(datasets)) {
    readmes <- readmes[ds_names %in% datasets, ]
    ds_names <- ds_names[ds_names %in% datasets]
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  staging_dir <- file.path(tempdir(), "peekbank_readmes_staging")
  local <- pb_download_files(readmes, staging_dir, skip_existing = FALSE)
  paths <- file.path(dir, paste0(ds_names, ".md"))
  for (i in seq_len(nrow(readmes))) {
    if (is.na(local[i])) {
      paths[i] <- NA_character_
      next
    }
    file.copy(local[i], paths[i], overwrite = TRUE)
  }
  unlink(staging_dir, recursive = TRUE)

  message("READMEs saved to: ", dir)
  stats::setNames(paths, ds_names)
}
