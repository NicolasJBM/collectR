#' @name update_market_and_financial_data
#' @title Create Excel files for market and financial data
#' @author Nicolas Mangin
#' @description Create Excel files for market and financial data based on companies in statements.
#' @param type Character. Whether the function should create a "market" or "financial" dataset.
#' @param path Character. Path to the folder where the dataset should be saved.
#' @return Excel files to be added to databases
#' @import acanva
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom furrr future_map
#' @importFrom future plan
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom writexl write_xlsx
#' @export

update_market_and_financial_data <- function(type = "market", path = NA){
  data <- NULL
  cik <- NULL

  if (type == "market"){
    if (!base::is.na(path)){
      path <- base::paste0(path, "/cost_of_capital.xlsx")
    } else path <- "cost_of_capital.xlsx"
    future::plan("multisession")
    cost_of_capital <- collectR::compute_cost_of_capital()
    writexl::write_xlsx(cost_of_capital, path = path)
  } else {
    if (!base::is.na(path)){
      path <- base::paste0(path, "/financial_data.xlsx")
    } else path <- "financial_data.xlsx"
    future::plan("multisession")
    financial_data <- tibble::tibble(cik = base::unique(acanva::statements$cik)) |>
      dplyr::mutate(data = furrr::future_map(cik, acanva::compute_financial_data)) |>
      dplyr::select(-cik) |>
      tidyr::unnest(data)
    writexl::write_xlsx(financial_data, path = path)
  }
}


