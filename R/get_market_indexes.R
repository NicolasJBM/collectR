#' @name get_market_indexes
#' @title Get indexes
#' @author Nicolas Mangin
#' @description Get index levels for the NYSE, NASDAQ, and SP500.
#' @param start Character. Starting date
#' @param end Character. Ending date
#' @return Tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr starts_with
#' @importFrom quantmod getSymbols
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @export


get_market_indexes <- function(start = "2019-01-01", end = "2025-01-01"){
  
  index <- NULL
  
  NYSE <- quantmod::getSymbols("^NY", from = start, to = end, periodicity = 'daily', src='yahoo', auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("NY."), names_to = "index", values_to = "value") |>
    tidyr::separate(index, into = c("symbol","id"), sep= "\\.")
  GSPC <- quantmod::getSymbols("^GSPC", from = start, to = end, periodicity = 'daily', src='yahoo', auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("GSPC."), names_to = "index", values_to = "value") |>
    tidyr::separate(index, into = c("symbol","id"), sep= "\\.")
  IXIC <- quantmod::getSymbols("^IXIC", from = start, to = end, periodicity = 'daily', src='yahoo', auto.assign=FALSE) |>
    base::as.data.frame() |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("IXIC."), names_to = "index", values_to = "value") |>
    tidyr::separate(index, into = c("symbol","id"), sep= "\\.")
  indexes <- dplyr::bind_rows(NYSE, GSPC, IXIC) |>
    tidyr::pivot_wider(names_from = "id", values_from = "value", values_fill = 0)
  
  base::names(indexes) <- base::tolower(base::names(indexes))
  
  return(indexes)
}

