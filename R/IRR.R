#' @name IRR
#' @title Internal Rate of Return
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute the periodic Internal Rate of Return associated with a series of period cash flows.
#' @param CF Numeric vector. Set of periodic cash flows.
#' @return Numeric. Periodic Internal Rate of Return associated with a series of period cash flows.
#' @importFrom FinCal irr
#' @export


IRR <- function(CF = 0){
  FinCal::irr(CF)
}

