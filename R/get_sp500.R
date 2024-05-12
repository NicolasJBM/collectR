#' @name get_sp500
#' @title Retrieve S&P500 companies
#' @author Nicolas Mangin
#' @description Retrieve S&P500 companies from Wikipedia.
#' @return Tibble with the list of S&P 500 companies.
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest read_html
#' @importFrom stringr str_replace_all
#' @importFrom tibble as_tibble
#' @export

get_sp500 <- function(){

  CIK <- NULL
  Date.added <- NULL
  Founded <- NULL
  GICS.Sector <- NULL
  GICS.Sub.Industry <- NULL
  Headquarters.Location <- NULL
  Security <- NULL
  Symbol <- NULL
  ticker <- NULL

  rvest::read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") |>
    rvest::html_nodes("table[id='constituents']") |>
    rvest::html_table() |>
    base::data.frame() |>
    tibble::as_tibble() |>
    dplyr::rename(
      ticker = Symbol,
      name = Security,
      sector = GICS.Sector,
      industry = GICS.Sub.Industry,
      headquarter = Headquarters.Location,
      added = Date.added,
      cik = CIK,
      founded = Founded
    ) |>
    dplyr::mutate(ticker = stringr::str_replace_all(ticker, "[.]", "-"))
}
