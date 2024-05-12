#' @name get_periodic_stock_prices
#' @title Retrieve stock prices
#' @author Nicolas Mangin
#' @description Retrieve stock prices for a vector of corporations' tickers.
#' @param tickers Character vector. List of tickers.
#' @param mindate Character. Beginning date ("2020-01-01)
#' @param maxdate Character. Ending date ("2022-12-01)
#' @param period Character. "monthly" or "daily".
#' @return Tibble. ticker, date, adjusted price.
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
#' @importFrom purrr map
#' @importFrom quantmod getSymbols
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


get_periodic_stock_prices <- function(tickers, mindate, maxdate, period = "monthly"){

  ticker <- NULL
  prices <- NULL

  tibble::tibble(
    ticker = tickers
  ) |>
    dplyr::mutate(
      prices = purrr::map(ticker, function(x){
        quantmod::getSymbols(
          x,
          from = mindate, to = maxdate, periodicity = period,
          src='yahoo', auto.assign=FALSE
        )
      })
    ) |>
    dplyr::mutate(prices = purrr::map(prices, function(x) {
      y <- x[,6] |>
        base::as.data.frame() |>
        tibble::rownames_to_column("date") |>
        dplyr::mutate(date = lubridate::as_date(date))
      base::names(y) <- c("date","adjusted_price")
      y
    })) |>
    tidyr::unnest(prices)
}
