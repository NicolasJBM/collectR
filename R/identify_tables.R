#' @name identify_tables
#' @title Find statements positions
#' @author Nicolas Mangin
#' @description Identify tables which are the most likely to be the desired financial statements.
#' @param tables List. all tables from a 10-K annual report.
#' @param all_patterns List of character vectors. Patterns identifying the statements.
#' @param remove Numeric vector. Position of the tables to be removed.
#' @param minscore Integer. Minimum number of pattern occurrences to keep a table as a candidate for a statement.
#' @param window Integer. To keep candidates having the best scores or scores near the best scores.
#' @return List of positions.
#' @export

identify_tables <- function(tables, all_patterns, remove = NA, minscore = 3, window = 2){
  
  patterns_rights <- all_patterns[[1]]
  patterns_duties <- all_patterns[[2]]
  patterns_income <- all_patterns[[3]]
  patterns_cash <- all_patterns[[4]]
  
  if (base::length(tables) >= 3){
    position_rights <- collectR::return_statement_position(tables, patterns_rights, remove = remove, minscore = minscore, window = window)
    position_duties <- collectR::return_statement_position(tables, patterns_duties, remove = remove, minscore = minscore, window = window)
    position_income <- collectR::return_statement_position(tables, patterns_income, remove = remove, minscore = minscore, window = window)
    position_cash <- collectR::return_statement_position(tables, patterns_cash, remove = remove, minscore = minscore, window = window)
  } else {
    position_rights <- base::character(0)
    position_duties <- base::character(0)
    position_income <- base::character(0)
    position_cash <- base::character(0)
  }
  
  if (base::length(position_rights) > 0 &
      base::length(position_duties) > 0 &
      base::length(position_income) > 0 &
      base::length(position_cash) > 0 ){
    if (base::length(base::intersect(position_rights, position_duties)) == 1){
      position_rights <- base::intersect(position_rights, position_duties)
      position_duties <- base::intersect(position_rights, position_duties)
    }
    
    if (base::length(position_rights) == 1){
      position_reference <-position_rights
    } else if (base::length(position_duties) == 1){
      position_reference <-position_duties
    } else if (base::length(position_income) == 1){
      position_reference <-position_income
    } else if (base::length(position_cash) == 1){
      position_reference <-position_cash
    } else {
      position_reference <- base::sample(position_cash, 1)
    }
    
    if (base::length(position_rights) != 1){
      position_rights <- position_rights[base::match(
        base::abs(base::as.numeric(position_rights)-base::as.numeric(position_reference)),
        min(base::abs(base::as.numeric(position_rights)-base::as.numeric(position_reference))), 0
      )==1]
    }
    
    if (base::length(position_duties) != 1){
      position_duties <- position_duties[base::match(
        base::abs(base::as.numeric(position_duties)-base::as.numeric(position_reference)),
        min(base::abs(base::as.numeric(position_duties)-base::as.numeric(position_reference))), 0
      )==1]
    }
    
    if (base::length(position_rights) != 1 & base::length(position_duties) == 1){
      position_rights <- position_rights[base::match(
        base::as.numeric(position_rights)-base::as.numeric(position_duties),
        min(base::as.numeric(position_rights)-base::as.numeric(position_duties)), 0
      )==1]
    }
    
    if (base::length(position_duties) != 1 & base::length(position_rights) == 1){
      position_duties <- position_duties[base::match(
        base::as.numeric(position_rights)-base::as.numeric(position_duties),
        min(base::as.numeric(position_rights)-base::as.numeric(position_duties)), 0
      )==1]
    }
    
    if (base::length(position_income) != 1){
      position_income <- position_income[base::match(
        base::abs(base::as.numeric(position_income)-base::as.numeric(position_reference)),
        min(base::abs(base::as.numeric(position_income)-base::as.numeric(position_reference))), 0
      )==1]
    }
    
    if (base::length(position_cash) != 1){
      position_cash <- position_cash[base::match(
        base::abs(base::as.numeric(position_cash)-base::as.numeric(position_reference)),
        min(base::abs(base::as.numeric(position_cash)-base::as.numeric(position_reference))), 0
      )==1]
    }
    
    unique_statements <- base::length(base::unique(c(position_rights, position_duties, position_income, position_cash)))
  } else {
    position_rights <- c("")
    position_duties <- c("")
    position_income <- c("")
    position_cash <- c("")
  }
  
  positions <- base::list(
    position_rights, position_duties, position_income, position_cash
  )
  
  return(positions)
}

