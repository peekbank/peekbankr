# network tests need internet + redivis auth
skip_network <- function() {
  skip_on_cran()
  skip_if_not_installed("redivis")
  skip_if(!identical(Sys.getenv("PEEKBANK_NETWORK_TESTS"), "true"),
          "PEEKBANK_NETWORK_TESTS not set")
}
