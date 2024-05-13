#' @name get_share_number
#' @title Get share number
#' @author Nicolas Mangin
#' @description Retrieve the number of shares from a HTML annual report.
#' @param html_file Character. Path to the local HTML file.
#' @return Integer. Number of shares.
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_remove_all
#' @importFrom xml2 read_html
#' @export


get_share_number <- function(html_file){
  shares <- xml2::read_html(html_file) |>
    rvest::html_nodes(xpath='//*[@name="dei:EntityCommonStockSharesOutstanding"]') |>
    rvest::html_text() |>
    stringr::str_remove_all(",") |>
    stringr::str_remove_all("[a-z]") |>
    stringr::str_remove_all("[A-Z]") |>
    base::as.numeric()
  
  if (base::length(shares) == 0){
    base::as.numeric(NA)
  } else {
    base::max(shares, na.rm = TRUE)
  }
}


