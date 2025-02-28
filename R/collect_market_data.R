#' @name get_market_data
#' @title Collect market data
#' @author Nicolas Mangin
#' @description Collect stock prices, dividends, market capitalization and number of common outstanding shares for a company.
#' @param symbol Character. Ticker of the company.
#' @param fmpkey Character. API key for FMP.
#' @param start Character. Starting date.
#' @param end Character. Ending date.
#' @return Tibble.
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_all
#' @importFrom dplyr select
#' @importFrom jsonlite fromJSON
#' @importFrom quantmod getDividends
#' @importFrom quantmod getSymbols
#' @importFrom rvest html_text
#' @importFrom rvest read_html
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr replace_na
#' @export


get_market_data <- function(
    symbol = "AAPL",
    fmpkey = NA,
    start = "2019-01-01",
    end = "2024-12-31"
){
  
  adjusted <- NULL
  div <- NULL
  high <- NULL
  low <- NULL
  marketcap <- NULL
  ncso <- NULL
  volume <- NULL
  
  market_capitalization_raw <- base::paste0(
    "https://financialmodelingprep.com/api/v3/historical-market-capitalization/",
    symbol, "?from=",start,"&to=",end,"", "&apikey=", fmpkey
  ) |>
    rvest::read_html() |>
    rvest::html_text() |>
    jsonlite::fromJSON() |>
    stats::na.omit()
  
  base::Sys.sleep(1)
  
  prices <- quantmod::getSymbols(
    symbol, from = start, to = end,
    periodicity = 'daily',
    src='yahoo', auto.assign=FALSE
  ) |>
    stats::na.omit()
  
  base::Sys.sleep(1)
  
  dividends <- quantmod::getDividends(
    symbol, from = start, to = end,
    src = "yahoo", auto.assign=FALSE
  ) |>
    stats::na.omit()
  
  base::Sys.sleep(1)
  
  if (base::length(dividends) > 0){
    market <- dplyr::left_join(
      prices |> base::as.data.frame() |> tibble::rownames_to_column("date"),
      dividends |> base::as.data.frame() |> tibble::rownames_to_column("date"),
      by = "date"
    ) |>
      dplyr::rename_all(stringr::str_remove_all, base::paste0("^",symbol,".")) |>
      tidyr::replace_na(base::list(div = 0))
  } else {
    market <- prices |>
      base::as.data.frame() |>
      tibble::rownames_to_column("date") |>
      dplyr::rename_all(stringr::str_remove_all, base::paste0("^", symbol,".")) |>
      dplyr::mutate(div = 0)
  }
  
  base::Sys.sleep(1)
  
  market <- market |>
    tidyr::replace_na(base::list(div = 0)) |>
    dplyr::left_join(
      market_capitalization_raw,
      by = "date"
    ) |>
    dplyr::rename_all(base::tolower) |>
    dplyr::mutate(ncso = marketcap/close) |>
    tidyr::replace_na(base::list(symbol = symbol)) |>
    dplyr::select(
      symbol, date, open, high, low, close,
      volume, adjusted, marketcap, ncso, div
    ) |>
    dplyr::mutate(
      div = base::round(div,2),
      ncso = base::round(ncso/10,0)*10
    )
  
  base::Sys.sleep(1)
  
  return(market)
}
