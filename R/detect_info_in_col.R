#' @name detect_info_in_col
#' @title FDetect information
#' @author Nicolas Mangin
#' @description Check whether a column contains some pieces of information.
#' @param x Character vector.
#' @return Logical. Whether the character vector contains some pieces of information.
#' @importFrom stringr str_detect
#' @export


detect_info_in_col <- function(x) base::any(stringr::str_detect(x, "[0-Z]"), na.rm = TRUE)

