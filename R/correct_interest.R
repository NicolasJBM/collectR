#' @name correct_interest
#' @title Correct interest
#' @author Nicolas Mangin
#' @description Increase the interest expense and acknowledge a financial gain to account for the gap betweon interest expense and interest paid.
#' @param x Tibble. Statements filtered on INT and PAIDINT.
#' @return Character. ID for the account.
#' @importFrom tibble tibble
#' @export


correct_interest <- function(x){
  
  if (base::all("INT" %in% x$id, "PAIDINT" %in% x$id, base::nrow(x) == 2)){
    
    y <- x$amount
    base::names(y) <- x$id
    
    difference <- y["PAIDINT"] + y["INT"]
    
    if (difference > 0){
      FINGAIN <- difference
      INT = -difference
      
      z <- tibble::tibble(
        id = c("FINGAIN","INT"),
        label  = c("Financial gains (losses) and interest income", "Interest expense"),
        amount = c(FINGAIN, INT)
      )
      
    } else {
      z <- tibble::tibble(
        id = c("FINGAIN","INT"),
        label  = c("Financial gains (losses) and interest income", "Interest expense"),
        amount = c(0, 0)
      )
    }
    
  } else if (base::all("PAIDINT" %in% x$id, base::nrow(x) == 1)){
    
    y <- x$amount
    base::names(y) <- x$id
    
    difference <- y["PAIDINT"]
    FINGAIN <- difference
    INT = -difference
    z <- tibble::tibble(
      id = c("FINGAIN","INT"),
      label  = c("Financial gains (losses) and interest income", "Interest expense"),
      amount = c(FINGAIN, INT)
    )
    
  } else {
    z <- tibble::tibble(
      id = c("FINGAIN","INT"),
      label  = c("Financial gains (losses) and interest income", "Interest expense"),
      amount = c(0, 0)
    )
  }
  
  return(z)
}