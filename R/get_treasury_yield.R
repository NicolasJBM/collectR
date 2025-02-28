#' @name get_treasury_yield
#' @title Get US risk-gree rate
#' @author Nicolas Mangin
#' @description Collect US treasury yield data.
#' @param start Character. Starting date
#' @param end Character. Ending date
#' @return Tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr starts_with
#' @importFrom quantmod getSymbols
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @export


get_treasury_yield <- function(start = "2019-01-01", end = "2025-01-01"){
  usty01 <- quantmod::getSymbols("DGS1", src = "FRED", from = start, to = end, auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  usty02 <- quantmod::getSymbols("DGS2", src = "FRED", from = start, to = end, auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  usty05 <- quantmod::getSymbols("DGS5", src = "FRED", from = start, to = end, auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  usty10 <- quantmod::getSymbols("DGS10", src = "FRED", from = start, to = end, auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  usty20 <- quantmod::getSymbols("DGS20", src = "FRED", from = start, to = end, auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  usty30 <- quantmod::getSymbols("DGS30", src = "FRED", from = start, to = end, auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date")
  
  dplyr::left_join(usty01, usty02, by = "date") |>
    dplyr::left_join(usty05, by = "date") |>
    dplyr::left_join(usty10, by = "date") |>
    dplyr::left_join(usty20, by = "date") |>
    dplyr::left_join(usty30, by = "date") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("DGS"), names_to = "id", values_to = "value") |>
    stats::na.omit()
}
