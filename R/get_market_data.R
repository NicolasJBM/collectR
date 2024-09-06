#' @name get_market_data
#' @title Get data about stock price and dividends
#' @author Nicolas Mangin
#' @param filings Tibble. Cik codes and date.
#' @description Get data about stock price and dividends
#' @return Table with cik, date, adjusted_price and dividend.
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom lubridate as_date
#' @importFrom purrr map
#' @importFrom quantmod getDividends
#' @importFrom quantmod getSymbols
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @export


get_market_data <- function(filings){

  adjusted_price <- NULL
  cik <- NULL
  data <- NULL
  dividend <- NULL
  ticker <- NULL
  
  companies <- filings |>
    dplyr::group_by(cik) |>
    dplyr::mutate(
      mindate = base::min(date),
      maxdate = base::max(date),
    ) |>
    dplyr::ungroup() |>
    dplyr::select(cik, mindate, maxdate) |>
    base::unique() |>
    dplyr::left_join(collectR::corporations, by = "cik") |>
    dplyr::select(cik, ticker, mindate, maxdate) |>
    base::unique()

  mindate <- base::min(companies$mindate)
  maxdate <- base::max(companies$maxdate)

  daily_financial_data <- tibble::tibble(
    ticker = companies$ticker
  ) |>
    dplyr::mutate(
      data = purrr::map(ticker, function(x, from, to){
        quantmod::getSymbols(
          x,
          from = from, to = to, periodicity = 'daily',
          src='yahoo', auto.assign=FALSE
        )
      },
      from = mindate, to = maxdate)
    ) |>
    dplyr::mutate(data = purrr::map(data, function(x) {
      y <- x[,6] |>
        base::as.data.frame() |>
        tibble::rownames_to_column("date") |>
        dplyr::mutate(date = lubridate::as_date(date))
      base::names(y) <- c("date","adjusted_price")
      y
    })) |>
    tidyr::unnest(data)

  dividends <- companies |>
    dplyr::select(ticker) |>
    dplyr::mutate(
      data = purrr::map(ticker, function(ticker, from, to){
        div <- quantmod::getDividends(
          ticker,
          from = from,
          to = to,
          src = "yahoo",
          auto.assign = TRUE,
          auto.update = TRUE,
          verbose = FALSE
        ) |>
          base::as.data.frame() |>
          tibble::rownames_to_column("date")
        base::names(div) <- c("date","dividend")
        div
      }, from = mindate, to = maxdate)
    ) |>
    tidyr::unnest(data)

  market_data <- daily_financial_data |>
    dplyr::mutate(date = base::as.character(date)) |>
    dplyr::full_join(dividends, by = c("ticker","date")) |>
    tidyr::replace_na(base::list(dividend = 0)) |>
    dplyr::left_join(dplyr::select(companies, ticker, cik), by = "ticker") |>
    dplyr::select(cik, date, adjusted_price, dividend)
  
  return(market_data)
}

