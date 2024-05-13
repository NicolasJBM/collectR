#' @name detect_fs_pattern
#' @title Detect pattern
#' @author Nicolas Mangin
#' @description Check whether a character vector contains a specific pattern.
#' @param x Character vector.
#' @param y Character. A pattern to detect.
#' @return Logical. Whether the character vector contains a specific pattern.
#' @importFrom stringr str_detect
#' @export

detect_fs_pattern <- function(x, y) base::any(stringr::str_detect(base::tolower(base::unlist(x[1])), y), na.rm = TRUE)
