#' @name complete_cik
#' @title Standardize cik format
#' @author Nicolas Mangin
#' @description add CIK and ensures a 10 digits cik code.
#' @param cik Character. Short CIK code
#' @return Character. Long CIK code
#' @export


complete_cik <- function(cik){
  l <- base::nchar(cik)
  base::paste0(
    "CIK",
    base::paste(base::rep(0, 10-l), collapse = ""),
    cik,
    collapse = ""
  )
}
