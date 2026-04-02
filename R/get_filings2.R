

get_filings <- function(symbol, start, end, fmpkey){
  get_corporation_filings <- function(symbol, start, end, fmpkey){
    base::paste0(
      "https://financialmodelingprep.com/stable/sec-filings-search/symbol?symbol=",
      symbol, "&from=",start,"&to=",end,"&page=0&limit=100&apikey=", fmpkey
    ) |>
      rvest::read_html() |>
      rvest::html_text() |>
      jsonlite::fromJSON() |>
      dplyr::filter(formType == "10-K") |>
      dplyr::select(filingDate, acceptedDate, link, finalLink) |>
      dplyr::mutate(
        filingDate = lubridate::ymd(base::as.Date(filingDate)),
        acceptedDate = lubridate::ymd(base::as.Date(acceptedDate))
      ) |>
      dplyr::mutate_all(base::as.character)
  }
  
  safe_get_corporation_filings <- purrr::safely(get_corporation_filings)
  
  filings <- corporations |>
    dplyr::select(symbol) |>
    dplyr::mutate(filings = purrr::map(symbol, safe_get_corporation_filings, start, end, fmpkey, .progress = TRUE)) |>
    dplyr::mutate(filings = purrr::map(filings, function(x) x$result)) |>
    tidyr::unnest(filings) |>
    dplyr::select(symbol, date = acceptedDate, link = finalLink) |>
    dplyr::left_join(dplyr::select(corporations, symbol, cik), by = "symbol") |>
    dplyr::mutate(htmlfile = base::paste0("CIK", cik, "_10-K_", date))
  
  base::save(filings, file = "data/filings.RData")
}