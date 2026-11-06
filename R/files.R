# the files of a connection's release as (file_id, file_name, size), named
# <dataset>/README.md and <dataset>/raw_data/<path>
list_dataset_files <- function(con) {
  redivis_tag <- files_redivis_tag(con)
  files <- if (is.null(redivis_tag)) NULL else pb_files_listing(redivis_tag)
  # the empty tibble keeps the columns when there are no files
  dplyr::bind_rows(
    dplyr::tibble(file_id = character(), file_name = character(), size = numeric()),
    files)
}

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

  # paths are relative to the dataset's raw_data/; strip leading "/", as that is not worth failing an import over
  wanted <- stimuli_df %>%
    dplyr::mutate(full_stimulus_path = paste0(
      .data$dataset_name, "/", sub("^/", "", .data$stimulus_image_path)))

  # Datasets of older releases that are not in the release holding the files
  # (e.g. renamed ones) count as missing. Their image paths were not
  # consistent anyway, so there isnt much lost.
  files <- list_dataset_files(con) %>%
    dplyr::filter(grepl("^[^/]+/raw_data/", .data$file_name)) %>%
    # saved as <dataset>/<path>, without the raw_data/ they have in the release
    dplyr::mutate(file_name = sub("^([^/]+)/raw_data/", "\\1/", .data$file_name)) %>%
    dplyr::filter(.data$file_name %in% wanted$full_stimulus_path)

  missing <- setdiff(wanted$full_stimulus_path, files$file_name)
  if (length(missing) > 0) {
    message(length(missing), " stimulus files were not found in the ",
            "files of the release (e.g. ", missing[1], ")")
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

  paths <- download_readmes(con, datasets_df$dataset_name, dir)
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

# the READMEs of the named datasets (every one in the release if NULL), saved
# as <dataset>.md under dir; returns their local paths named by dataset, NA
# where a download failed
download_readmes <- function(con, datasets, dir) {
  readmes <- list_dataset_files(con) %>%
    dplyr::filter(grepl("^[^/]+/README\\.md$", .data$file_name))
  if (!is.null(datasets)) {
    readmes <- readmes[sub("/README\\.md$", "", readmes$file_name) %in% datasets, ]
  }
  ds_names <- sub("/README\\.md$", "", readmes$file_name)

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
