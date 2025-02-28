#' @name get_fs_files
#' @title Retrieve the content of csv files.
#' @author Nicolas Mangin
#' @description Retrieve the content of csv files.
#' @param folder Character. Path to csv files.
#' @return Tibble. Content of csv files.
#' @importFrom dplyr bind_rows
#' @importFrom furrr future_map
#' @export


get_fs_files <- function(folder){
  folder |>
    base::list.files(full.names = TRUE) |>
    furrr::future_map(readr::read_csv, col_types = "cDcDccn") |>
    dplyr::bind_rows() |>
    stats::na.omit()
}
