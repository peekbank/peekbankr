# stimulus images for a release, saved under local_base_dir; returns the
# stimuli table with the local path of each image
fetch_stimulus_images <- function(con, local_base_dir = "stimulus_data",
                                  datasets = c(), skip_existing = TRUE) {
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

  local <- pb_download_files(files, local_base_dir,
                             skip_existing = skip_existing)
  path_map <- stats::setNames(local, files$file_name)
  message("Downloaded ", sum(!is.na(local)), " stimulus files from Redivis")

  return(wanted %>%
    dplyr::mutate(local_stimulus_path =
                    unname(path_map[.data$full_stimulus_path])) %>%
    dplyr::select(-"full_stimulus_path"))
}

# one README per dataset, saved as <dataset>.md under local_base_dir
fetch_readmes <- function(datasets = c(), local_base_dir = "dataset_readmes") {
  # READMEs live at <dataset>/README.md in the peekbank_files dataset
  index <- pb_files_query("")
  if (is.null(index)) return(invisible(NULL))
  readmes <- index[grepl("^[^/]+/README\\.md$", index$file_name), ]
  if (length(datasets) > 0) {
    readmes <- readmes[sub("/README\\.md$", "", readmes$file_name) %in%
                         datasets, ]
  }

  if (!dir.exists(local_base_dir)) dir.create(local_base_dir, recursive = TRUE)

  staging_dir <- file.path(tempdir(), "peekbank_readmes_staging")
  local <- pb_download_files(readmes, staging_dir, skip_existing = FALSE)
  for (i in seq_len(nrow(readmes))) {
    if (is.na(local[i])) next
    ds_name <- sub("/README\\.md$", "", readmes$file_name[i])
    file.copy(local[i], file.path(local_base_dir, paste0(ds_name, ".md")),
              overwrite = TRUE)
  }
  unlink(staging_dir, recursive = TRUE)

  message("READMEs saved to: ", local_base_dir)
}
