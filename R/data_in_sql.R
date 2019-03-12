#' Embed CSV data in SQL
#'
#' Translate a dataset into a plaintext SQL representation
#'
#' @param data dataset to be translated
#' @param tab_name name of the resulting "WITH table".
#' @param separator specifies string to be used as delimiter in the SQL query (use any character/string that is not present in the data).
#'
#' @details
#' Tested with Oracle's PL/SQL.
#'
#' @return SQL query as a character value.
#' @examples
#' # data(iris)
#' # cat(csv_in_sql(iris))
#' @export
csv_in_sql <- function(data,
                       tab_name = "tbl",
                       separator = "|",
                       dialect = "PLSQL",
                       version = c("11g", "10g"),
                       na.string = "") {

  stopifnot(tolower(dialect) == "plsql")
  version <- match.arg(version)

  # data <- apply(data, 2, as.character)
  data[is.na(data)] <- na.string

  # concatenate columns into char-separated string
  char_str <- sapply(data, paste, USE.NAMES = TRUE, collapse = separator)

  data_part <-
    paste0(
      paste(dbtools::squote(char_str), " AS ", names(char_str)),
      collapse = ",\n      "
    )

  regexp_part <-
    paste0(
      if (version >= "11g")
        paste0(          "  regexp_substr(", names(char_str), stringr::str_interp(", '(.*?)([${separator}]|$)', 1, level, null, 1)"),                   " AS ", names(char_str)) else
          paste0("  replace(regexp_substr(", names(char_str), stringr::str_interp(", '(.*?)([${separator}]|$)', 1, level, null), '${separator}', '')"), " AS ", names(char_str)) ,
      collapse = ",\n      ")

  q <- stringr::str_interp(
    "
WITH
  ${tab_name}_concat AS (
    SELECT
      ${data_part}
    FROM DUAL
 ),
${tab_name} AS (
    SELECT
      ${regexp_part}
    FROM ${tab_name}_concat
    CONNECT BY level <= ${nrow(data)})
    -- CONNECT BY level <= regexp_count(${names(char_str)[1]}, '[${separator}]')+1)
select * from ${tab_name}
")

  return(q)
}
