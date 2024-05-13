#' @name score_fs_patterns
#' @title Score patterns
#' @author Nicolas Mangin
#' @description Count the occurrences of a term in a table to identify whether the table is one of the financial statements.
#' @param x Matrix. Table in which the terms should be counted.
#' @param y Character vector. Terms or expressions which should be counted in y.
#' @return Integer. Count of occurrences of any term from y in x.
#' @importFrom stringr str_count
#' @export


score_fs_patterns <- function(x, y) stringr::str_count(base::tolower(base::paste(base::unlist(x[1]), collapse = " ")), base::paste(y, collapse = "|"))
