#' Download specific peekbank dataset from OSF
#'
#' @param lab_dataset_id Specific ID occurring in the file hierarchy of the
#'   relevant OSF repo.
#' @param path Where you want it on your own machine. Will error if directory
#'   doesn't exist.
#' @param osf_address pr6wu for peekbank.
#' @export
ds.get_raw_data <- function(lab_dataset_id, path = ".", osf_address = "pr6wu") {

  # get file list in the relevant raw data directory and download
  osfr::osf_retrieve_node(osf_address) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(.data$name == lab_dataset_id) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(.data$name == "raw_data") %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    osfr::osf_download(path = path,
                       conflicts = "overwrite", verbose = TRUE, progress = TRUE)
}

#' Download peekbank processed dataset from OSF
#'
#' @param lab_dataset_id Specific ID occurring in the file hierarchy of the
#'   relevant OSF repo.
#' @param path Where you want it on your own machine. Will error if directory
#'   doesn't exist.
#' @param osf_address pr6wu for peekbank.
#' @export
ds.get_processed_data <- function(lab_dataset_id, path = ".",
                               osf_address = "pr6wu") {
  # check if path exists, if not, create path
  if (!file.exists(path)) {
    dir.create(file.path(path), showWarnings = FALSE)
  }

  # get file list in the relevant raw data directory and download
  osfr::osf_retrieve_node(osf_address) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(.data$name == lab_dataset_id) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(.data$name == "processed_data") %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    osfr::osf_download(path = path, conflicts = "overwrite", verbose = TRUE,
                       progress = TRUE)
}

#' Put processed data for specific peekbank dataset on OSF
#'
#' @param token personal access tokens for uploading to OSF
#' @param dataset_name Specific dataset name occurring in the file hierarchy of
#'   the relevant OSF repo.
#' @param path Where the data live on your own machine.
#' @param osf_address pr6wu for peekbank.
#' @export
ds.put_processed_data <- function(token, dataset_name, path = ".",
                               osf_address = "pr6wu") {
  osfr::osf_auth(token = token)

  osfr::osf_retrieve_node(osf_address) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(.data$name == dataset_name) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(.data$name == "processed_data") %>%
    osfr::osf_upload(path = stringr::str_c(path,
                                           list.files(path = path,
                                                      recursive = TRUE)),
                     recurse = TRUE, conflicts = "overwrite", verbose = TRUE,
                     progress = TRUE)
}
