#' @name get_us_lt_rates
#' @title Retrieve US long-term interest rates
#' @author Nicolas Mangin
#' @description Retrieve US long-term interest rates
#' @return Tibble. period, value
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rdbnomics rdb
#' @export


get_us_lt_rates <- function(){

  period <- NULL
  value <- NULL

  rdbnomics::rdb("OECD/DP_LIVE/USA.LTINT.TOT.PC_PA.Q") |>
    base::as.data.frame() |>
    dplyr::select(period, value) |>
    dplyr::mutate(period = base::as.character(period))
}
