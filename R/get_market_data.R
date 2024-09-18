#' @name get_market_data
#' @title Get data about stock price and dividends
#' @author Nicolas Mangin
#' @param cik Character. cik code of the company
#' @param ticker Character. Ticker of the company.
#' @param from Character. IDO code for the first date.
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


get_market_data <- function(cik = NA, ticker = NA, from = NA){

  base::stopifnot(
    !base::is.na(cik),
    !base::is.na(ticker),
    !base::is.na(from)
  )
  
  base::Sys.sleep(2)
  base::print(base::paste0(ticker, " - ", cik))
  
  adjusted_price <- NULL
  data <- NULL
  dividend <- NULL
  
  daily_financial_data <- quantmod::getSymbols(
    ticker,
    from = from,
    periodicity = 'daily',
    src='yahoo',
    auto.assign = FALSE
  ) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date") |>
    dplyr::mutate(date = lubridate::as_date(date)) |>
    dplyr::select(date, dplyr::ends_with("Adjusted"))
  base::names(daily_financial_data) <- c("date","adjusted_price")

  dividends <- quantmod::getDividends(
    ticker,
    from = from,
    src = "yahoo",
    auto.assign = FALSE,
    auto.update = TRUE,
    verbose = FALSE
  ) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  base::names(dividends) <- c("date","dividend")

  market_data <- daily_financial_data |>
    dplyr::mutate(
      cik = cik,
      date = base::as.character(date)
    ) |>
    dplyr::full_join(dividends, by = c("date")) |>
    tidyr::replace_na(base::list(dividend = 0)) |>
    dplyr::select(cik, date, adjusted_price, dividend)
  
  return(market_data)
}

