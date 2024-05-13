#' @name get_tables_from_html
#' @title Retrieve HTML tables
#' @author Nicolas Mangin
#' @description Retrieve all the tables from a HTML 10-K annual report.
#' @param file Character. Path to the local copy of the HTML file.
#' @return List of tables.
#' @importFrom XML htmlParse
#' @importFrom XML readHTMLTable
#' @export


get_tables_from_html <- function(file){
  tables <- XML::htmlParse(file)
  tables <- XML::readHTMLTable(tables)
  base::names(tables) <- base::as.character(base::seq_len(base::length(tables)))
  return(tables)
}

