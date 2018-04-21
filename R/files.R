#' Read SQL code from files
#' @param fp file path
#' @param ... other arguments to file()
#' @export
read_sql_file <- function(fp, ...) {
  file_con <- file(fp, ...)
  sql_text <- paste(readLines(file_con), collapse = "\n")
  close(file_con)

  return(sql_text)
}

