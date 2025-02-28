#' @name get_filings
#' @title Classify labels in financial statements
#' @author Nicolas Mangin
#' @description Assign an account ID to financial statements accounts based on a preliminary classification and the terms appearing in the label.
#' @param folder Character. path to the folder containing filings lists.
#' @return Character. ID for the account.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


get_filings <- function(folder){
  
  json_file <- NULL
  data <- NULL
  
  tibble::tibble(
    json_file = base::list.files(folder, full.names = TRUE)
  ) |>
    dplyr::mutate(data = purrr::map(json_file, collectR::flatten_submission)) |>
    tidyr::unnest(data) |>
    dplyr::select(-json_file)
}
