#' @name flatten_submission
#' @title Flatten submission
#' @author Nicolas Mangin
#' @description Transform a SEC JSON file into a tibble with the relevant information.
#' @param json_file Character. Path to the local json file.
#' @return Tibble. Submissions information.
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @export


flatten_submission <- function(json_file){
  
  accessionNumber <- NULL
  primaryDocument <- NULL
  form <- NULL
  filingDate <- NULL
  reportDate <- NULL
  link <- NULL
  
  submissions <- jsonlite::fromJSON(json_file, flatten=TRUE)
  cik <- json_file |>
    stringr::str_remove_all("^data/json/") |>
    stringr::str_remove_all(".json$")
  
  submissions$filings$recent |>
    base::as.data.frame() |>
    dplyr::mutate(
      cik = cik,
      link = base::paste0(
        "https://www.sec.gov/Archives/edgar/data/",
        stringr::str_remove(cik, "^CIK0+"), "/",
        stringr::str_remove_all(accessionNumber, "-"), "/",
        primaryDocument
      )
    ) |>
    dplyr::filter(form == "10-K") |>
    dplyr::select(cik, form, filingDate, reportDate, link) |>
    dplyr::arrange(cik, form, dplyr::desc(reportDate)) |>
    dplyr::mutate_all(base::as.character) |>
    base::as.data.frame()
}

