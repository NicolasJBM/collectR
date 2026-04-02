


get_profiles <- function(symbol, fmpkey){
  get_corporation_profile <- function(symbol, fmpkey){
    base::paste0(
      "https://financialmodelingprep.com/stable/profile?symbol=",
      symbol, "&apikey=", fmpkey
    ) |>
      rvest::read_html() |>
      rvest::html_text() |>
      jsonlite::fromJSON() |>
      dplyr::select(
        cik, isin, cusip, exchange,
        industry, description, sector, country, state, image, ipoDate,
        isEtf, isActivelyTrading, isAdr, isFund
      ) |>
      dplyr::mutate_all(base::as.character)
  }
  
  safe_get_corporation_profile <- purrr::safely(get_corporation_profile)
  
  corporations <- base::paste0(
    "https://financialmodelingprep.com/stable/financial-statement-symbol-list", "?apikey=", fmpkey
  ) |>
    rvest::read_html() |>
    rvest::html_text() |>
    jsonlite::fromJSON() |>
    stats::na.omit() |>
    dplyr::filter(reportingCurrency == "USD") |>
    dplyr::mutate(profiles = purrr::map(symbol, safe_get_corporation_profile, fmpkey, .progress = TRUE)) |>
    dplyr::mutate(profiles = purrr::map(profiles, function(x) x$result)) |>
    tidyr::unnest(profiles) |>
    dplyr::filter(isActivelyTrading == "TRUE") |>
    stats::na.omit()
  
  base::save(corporations, "data/corporations.RData")
}
