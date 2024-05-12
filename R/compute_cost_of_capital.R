#' @name compute_cost_of_capital
#' @title Compute cost of capital
#' @author Nicolas Mangin
#' @description Compute various components of the cost of capital.
#' @param tickers Character vector. Tickers of the companies for which the cost of capital should be computed.
#' @param market Character. Ticker of the market used as reference.
#' @return Tibble. cik, id, date, value
#' @importFrom broom tidy
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom furrr furrr_options
#' @importFrom furrr future_map
#' @importFrom furrr future_map2
#' @importFrom furrr future_map2_dbl
#' @importFrom future plan
#' @importFrom purrr map
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @export

compute_cost_of_capital <- function(tickers = NA, market = '^GSPC'){

  MP <- NULL
  MR <- NULL
  Rf <- NULL
  adjusted_price <- NULL
  cik <- NULL
  data <- NULL
  estimate <- NULL
  id <- NULL
  period <- NULL
  prices <- NULL
  startdate <- NULL
  term <- NULL
  ticker <- NULL
  value <- NULL
  variation <- NULL

  if (base::is.na(tickers)){
    tickers <- base::unique(acanva::corporations$ticker)
  }

  corporations <- dplyr::select(acanva::corporations, cik, ticker) |>
    dplyr::group_by(cik) |> dplyr::sample_n(1) |> dplyr::ungroup() |>
    base::unique()

  companies <- acanva::statements |>
    dplyr::select(cik, date) |>
    base::unique() |>
    dplyr::left_join(corporations, by = "cik") |>
    dplyr::filter(ticker %in% tickers) |>
    dplyr::mutate(startdate = base::as.character(base::as.Date(date)-365*5))

  mindate <- base::min(companies$startdate)
  maxdate <- base::max(companies$date)

  monthly_financial_data <- collectR::get_periodic_stock_prices(
    c(market, companies$ticker),
    mindate, maxdate, 'monthly'
  )

  risk_free_rate <- collectR::get_us_lt_rates()

  future::plan("multisession", workers = 16)

  filtered_raw_data <- companies |>
    dplyr::mutate(prices = furrr::future_map(ticker, function(x,y,z){
      dplyr::filter(z, ticker %in% c(x,y))
    }, y = market, z = monthly_financial_data)) |>
    dplyr::mutate(prices = furrr::future_map2(prices, date, function(x,y){
      dplyr::filter(x, date <= y)
    })) |>
    dplyr::mutate(prices = furrr::future_map2(prices, startdate, function(x,y){
      dplyr::filter(x, date >= y)
    })) |>
    dplyr::mutate(Rf = furrr::future_map2(date, startdate, function(x,y,z){
      dplyr::filter(z, period <= x, period >= y)
    }, z = risk_free_rate)) |>
    dplyr::mutate(Rf = furrr::future_map(Rf, function(x){
      x |>
        dplyr::select(value) |>
        stats::na.omit() |>
        dplyr::mutate(value = value/100) |>
        base::unlist() |>
        base::as.numeric() |>
        base::mean() |>
        base::round(4)
    })) |> tidyr::unnest(Rf)

  estimates <- filtered_raw_data |>
    dplyr::mutate(beta = furrr::future_map2_dbl(prices, ticker, function(x,y,market){
      if (y %in% x$ticker){
        x |>
          dplyr::group_by(ticker) |>
          dplyr::mutate(variation = (adjusted_price - dplyr::lag(adjusted_price))/dplyr::lag(adjusted_price)) |>
          dplyr::select(-adjusted_price) |>
          stats::na.omit() |>
          dplyr::group_by(ticker, date) |>
          dplyr::sample_n(1) |>
          dplyr::ungroup() |>
          tidyr::pivot_wider(names_from = ticker, values_from = variation) |>
          stats::na.omit() |>
          tidyr::pivot_longer(cols = dplyr::all_of(y), names_to = "ticker", values_to = "variation") |>
          dplyr::select(ticker, date, market = dplyr::all_of(market), variation) |>
          dplyr::arrange(ticker, date) |>
          dplyr::group_by(ticker) |>
          tidyr::nest() |>
          dplyr::mutate(data = purrr::map(data, function(x){
            broom::tidy(stats::lm(variation ~ market, data = x)) |>
              dplyr::filter(term == "market") |>
              dplyr::select(beta = estimate)
          })) |>
          tidyr::unnest(data) |>
          dplyr::ungroup() |>
          dplyr::select(-ticker) |>
          base::unlist() |>
          base::as.numeric()
      } else base::as.numeric(NA)
    }, market = market, .options = furrr::furrr_options(seed = TRUE))) |>
    dplyr::mutate(MR = furrr::future_map(prices, function(x){
      market_growth <- x |>
        dplyr::filter(ticker == market) |>
        dplyr::filter(date == base::min(date) | date == base::max(date)) |>
        dplyr::arrange(date)
      acanva::RATE(
        NPER = 5,
        PMT = 0,
        PV = -market_growth$adjusted_price[1],
        FV = market_growth$adjusted_price[2]
      )
    })) |>
    tidyr::unnest(MR)

  cost_of_capital <- estimates |>
    dplyr::select(-prices, -ticker, -startdate) |>
    dplyr::mutate(
      MP = MR - Rf,
      Ke = Rf + beta * MP
    ) |>
    tidyr::pivot_longer(cols = c("beta","Rf","MR","MP","Ke"), names_to = "id", values_to = "value") |>
    dplyr::mutate(value = base::round(value, 4)) |>
    dplyr::select(cik, id, date, value) |>
    dplyr::arrange(cik, date)

  return(cost_of_capital)
}

