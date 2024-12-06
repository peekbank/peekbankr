utils::globalVariables(".")

#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
#' @importFrom glue glue
NULL

# set package globals in this way to avoid messing up the workspace of the user
# loading the package
pkg_globals <- new.env()
pkg_globals$SAMPLE_RATE <- 40 # Hz
pkg_globals$SAMPLE_DURATION <- 1000 / pkg_globals$SAMPLE_RATE
pkg_globals$MAX_GAP_LENGTH <- .100 # S
pkg_globals$MAX_GAP_SAMPLES <- pkg_globals$MAX_GAP_LENGTH /
  (1 / pkg_globals$SAMPLE_RATE)
pkg_globals$SCHEMA_FILE <- file.path("https://raw.githubusercontent.com",
                                     "langcog/peekbank/master/static",
                                     "peekbank-schema.json")

pkg_globals$WORDBANK_ALLOWED_LANGUAGES = c("Danish",
                                           "English (American)",
                                           "English (Australian)",
                                           "English (British)",
                                           "French (French)",
                                           "French (Quebecois)",
                                           "German",
                                           "Italian",
                                           "Greek (Cypriot)",
                                           "Norwegian",
                                           "Portuguese (European)",
                                           "Russian",
                                           "Spanish (European)",
                                           "Spanish (Mexican)",
                                           "Swedish",
                                           "Turkish",
                                           "Croatian",
                                           "Czech",
                                           "Mandarin (Beijing)",
                                           "Mandarin (Taiwanese)",
                                           "Cantonese",
                                           "Hebrew",
                                           "Korean",
                                           "Catalan",
                                           "Dutch",
                                           "Hungarian",
                                           "Arabic (Saudi)",
                                           "Latvian",
                                           "Slovak",
                                           "Kiswahili",
                                           "American Sign Language",
                                           "British Sign Language",
                                           "Estonian",
                                           "Kigiriama",
                                           "Japanese")


.msg <- function(s) {
  strwrap(prefix = " ", initial = "", glue(s, .envir = parent.frame()))
}

# Get the mode (respecting NAs)
# https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
na_mode <- function(x) {
  if (all(is.na(x))) {
    return(as.character(NA))
  } else {
    x_nona <- x[!is.na(x)]

    ux <- unique(x_nona)
    x_mode <- ux[which.max(tabulate(match(x_nona, ux)))]

    return(x_mode)
  }
}

# Render the peekbank schema directly from github
render_schema <- function(x) {
  schema <- jsonlite::fromJSON(pkg_globals$SCHEMA_FILE)
  table_names <- schema[[2]]
  tables <- schema[[3]]

  for (i in 1:length(table_names)) {
    print(dplyr::as_tibble(tables[[i]][, 1:2])) # knitr::kable
  }
}
