#' @name format_statement
#' @title Format a statement
#' @author Nicolas Mangin
#' @description Format and combine the selected and retrieved tables for each financial statement is a list. 
#' @param statement List. 3 Financial statements: BS, IS, CFS.
#' @return Tibble. Statements.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @export


format_statement <- function(statement){
  
  account <- NULL
  
  for (i in 3:base::length(statement)) {
    statement[,i] <- base::trimws(stringr::str_remove_all(statement[,i], "[,]"))
    statement[,i] <- base::trimws(stringr::str_remove_all(statement[,i], "[)]$"))
    statement[,i] <- base::trimws(stringr::str_remove_all(statement[,i], "[$]"))
    statement[,i] <- stringr::str_remove_all(statement[,i], "\\s")
    statement[,i] <- stringr::str_replace_all(statement[,i], "^[(]", "-")
    statement[,i] <- base::replace(statement[,i], base::is.na(statement[,i]), "")
  }
  
  statement <- statement |>
    dplyr::filter(base::nchar(account, "width") > 0)
  
  iterations <- base::length(statement)-4
  
  for (l in 1:iterations){
    for (i in 1:base::nrow(statement)) {
      for (j in 3:(base::length(statement))-1){
        if (base::nchar(statement[i,j], "width") == 0 & base::nchar(statement[i,j+1], "width") > 0){
          statement[i,j] <- statement[i,(j+1)]
          statement[i,(j+1)] <- ""
        }
      }
    }
  }
  
  filled_columns <- base::apply(statement, 2, function(x) base::mean(base::nchar(x, "width"), na.rm = TRUE) != 0)
  
  statement <- statement[,filled_columns] |>
    dplyr::mutate_all(function(x) base::replace(x, x == "\032", "")) |>
    dplyr::mutate_all(function(x) base::replace(x, base::nchar(x, "width") == 0, NA))
  
  for (i in 3:base::length(statement)){
    statement[,i] <- base::as.numeric(statement[,i])
  }
  
  base::names(statement) <- c("statement", "label", base::paste0("year_", 1:(base::length(statement)-2)))
  
  return(statement)
}
