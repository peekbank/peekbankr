# the SQL a timepoint method builds, without running it
captured_sql <- function(expr) {
  sql <- NULL
  testthat::local_mocked_bindings(
    pb_query = function(connection, sql_text) {
      sql <<- sql_text
      NULL
    },
    .package = "peekbankr")
  force(expr)
  sql
}
