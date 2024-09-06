#' @name RATE
#' @title Periodic rate of return
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute the periodic rate of return for an annuity.
#' @param NPER Integer. Number of periods.
#' @param PMT Numeric. Cash flow at every period.
#' @param PV Numeric. Cash flow at the beginning of all the periods.
#' @param FV Numeric. Cash flow at the end of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric.Periodic rate of return for an annuity.
#' @importFrom FinCal irr
#' @export


RATE <- function(
    NPER = 0,
    PMT = 0,
    PV = 0,
    FV = 0,
    type = 0
){
  if (type == 0){
    CF <- c(PV,base::rep(PMT,NPER))
    CF[base::length(CF)] <- CF[base::length(CF)]+FV
  } else {
    CF <- c(base::rep(PMT,NPER),FV)
    CF[1] <- CF[1]+PV
  }
  FinCal::irr(CF)
}
