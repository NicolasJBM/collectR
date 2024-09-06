#' @name compute_share_yield
#' @title Compute Share yield
#' @author Nicolas Mangin
#' @description Compute the share yield or total share return (including both dividends and change in value)
#' @param dataset Character. CIK code of the company.
#' @return Tibble with financial data
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map2_chr
#' @importFrom purrr map2_dbl
#' @importFrom purrr map2_int
#' @importFrom purrr map_int
#' @importFrom tidyr nest
#' @importFrom tidyr replace_na
#' @export


compute_share_yield <- function(dataset = NA){

  adjusted_price <- NULL
  cashflow <- NULL
  data <- NULL
  days <- NULL
  endyear <- NULL
  form <- NULL
  id <- NULL
  periodic_rate <- NULL
  periods <- NULL
  reportDate <- NULL
  startyear <- NULL
  value <- NULL
  year <- NULL
  cik <- NULL

  yieldbase <- collectR::filings |>
    dplyr::filter(cik == dataset, form == "10-K") |>
    dplyr::select(cik, date = reportDate) |>
    dplyr::mutate(endyear = 1) |>
    dplyr::full_join((dplyr::filter(collectR::market_data, cik == dataset)), by = c("cik", "date")) |>
    tidyr::replace_na(base::list(endyear = 0)) |>
    dplyr::arrange(date)

  yieldbase$adjusted_price <- collectR::repeat_last(yieldbase$adjusted_price)
  yieldbase$dividend <- collectR::repeat_last(yieldbase$dividend)

  yieldbase <- yieldbase |>
    dplyr::mutate(
      sellprice = adjusted_price,
      buyprice = - dplyr::lag(adjusted_price),
      startyear = dplyr::lag(endyear)
    ) |>
    stats::na.omit() |>
    dplyr::mutate(
      year = base::cumsum(startyear),
      cashflow = dplyr::case_when(
        endyear == 1 ~ sellprice + dividend,
        startyear == 1 ~ buyprice + dividend,
        TRUE ~ dividend
      )
    ) |>
    dplyr::select(cik, date, year, cashflow) |>
    dplyr::group_by(cik, year) |>
    tidyr::nest() |>
    dplyr::mutate(
      periods = purrr::map_int(data, base::nrow),
      days = purrr::map2_int(data, periods, function(x,y){
        base::as.integer(base::as.Date(x$date[periods]) - base::as.Date(x$date[1]))
      }),
      date = purrr::map2_chr(data, periods, function(x,y) x$date[y]),
      periodic_rate = purrr::map2_dbl(data, periods, function(x,y){
        if (x$cashflow[1] < 0 & x$cashflow[y] > 0)
          collectR::IRR(x$cashflow) else
            NA
      })
    ) |>
    dplyr::mutate(
      source = "market",
      id = "R^i",
      value = base::round(days * periodic_rate, 4)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(cik, date, source, id, value) |>
    stats::na.omit()

  return(yieldbase)
}





