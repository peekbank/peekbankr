#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang ":="
#' @importFrom glue glue
NULL

utils::globalVariables(".")

SAMPLE_RATE <- 40 # Hz
SAMPLE_DURATION <- 1000 / SAMPLE_RATE # ms

.msg <- function(s) {
  strwrap(prefix = " ", initial = "", glue(s, .envir = parent.frame()))
}
