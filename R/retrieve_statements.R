#' @name retrieve_statements
#' @title Retrieve statements
#' @author Nicolas Mangin
#' @description Retrieve and format tables selected as financial statements.
#' @param tables List. all tables from a 10-K annual report.
#' @param positions List. Position of the tables containing financial statements,
#' @param forcebs Character vector. List of tables ids for balance sheet.
#' @param forceis Character vector. List of tables ids for income statement.
#' @param forcecfs Character vector. List of tables ids for cash flow statement.
#' @return List of formatted financial statements.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom purrr safely
#' @export


retrieve_statements <- function(
    tables, positions,
    forcebs, forceis, forcecfs
  ){
  
  V1 <- NULL
  statement <- NULL
  
  position_rights <- positions[[1]][1]
  position_duties <- positions[[2]][1]
  position_income <- positions[[3]][1]
  position_cash <- positions[[4]][1]
  
  
  
  safe_format_statement <- purrr::safely(collectR::format_statement)
  
  if (forcebs != "") {
    
    forcebs <- stringr::str_split(forcebs, pattern = " ", simplify = TRUE) 
    
    
  } else if (position_rights != "" & position_duties != ""){
    if (position_rights == position_duties){
      balancesheet <- tables[[position_rights]] |>
        dplyr::mutate_all(function(x) base::trimws(base::gsub("[$]","",x)))
      contain_info <- base::apply(balancesheet, 2, collectR::detect_info_in_col)
      if (base::sum(contain_info) > 2){
        balancesheet <- balancesheet[, base::as.logical(contain_info)] |>
          dplyr::mutate(statement = "BS")
        
        balancesheet <- balancesheet |>
          dplyr::filter(V1 != "") |>
          dplyr::select(statement, account = V1, dplyr::everything()) |>
          safe_format_statement()
        
        if (!base::is.null(balancesheet$result)){
          balancesheet <- balancesheet$result
        } else {
          balancesheet <- NA
        }
        
      } else balancesheet <- NA
    } else {
      rights <- tables[[position_rights]] |>
        dplyr::mutate_all(function(x) base::trimws(base::gsub("[$]","",x)))
      contain_info <- base::apply(rights, 2, collectR::detect_info_in_col)
      if (base::sum(contain_info) > 2){
        rights <- rights[, base::as.logical(contain_info)] |>
          dplyr::mutate(statement = "BS")
      } else rights <- NA
      duties <- tables[[position_duties]] |>
        dplyr::mutate_all(function(x) base::trimws(base::gsub("[$]","",x)))
      contain_info <- base::apply(duties, 2, collectR::detect_info_in_col)
      if (base::sum(contain_info) > 2){
        duties <- duties[, base::as.logical(contain_info)] |>
          dplyr::mutate(statement = "BS")
      } else duties <- NA
      
      if (base::length(rights) > 1 & base::length(duties) > 1){
        balancesheet <- dplyr::bind_rows(rights, duties)
        
        balancesheet <- balancesheet |>
          dplyr::filter(V1 != "") |>
          dplyr::select(statement, account = V1, dplyr::everything()) |>
          safe_format_statement()
        
        if (!base::is.null(balancesheet$result)){
          balancesheet <- balancesheet$result
        } else {
          balancesheet <- NA
        }
        
      } else balancesheet <- NA
      base::rm(rights, duties)
    }
    
  } else balancesheet <- NA
  
  if (forceis != "") {
    
    forceis <- stringr::str_split(forceis, pattern = " ", simplify = TRUE) 
    
  } else if (position_income != ""){
    income <- tables[[position_income]] |>
      dplyr::mutate_all(function(x) base::trimws(base::gsub("[$]","",x)))
    contain_info <- base::apply(income, 2, collectR::detect_info_in_col)
    if (base::sum(contain_info) > 2){
      income <- income[, base::as.logical(contain_info)] |>
        dplyr::mutate(statement = "IS")
      
      income <- income |>
        dplyr::filter(V1 != "") |>
        dplyr::select(statement, account = V1, dplyr::everything()) |>
        safe_format_statement()
      
      if (!base::is.null(income$result)){
        income <- income$result
      } else {
        income <- NA
      }
      
    } else income <- NA
  }
  
  if (forcecfs != "") {
    
    forcecfs <- stringr::str_split(forcecfs, pattern = " ", simplify = TRUE) 
    
    cash <- tables[[forcecfs[1]]]
    if (base::length(forcecfs) > 1) {
      for (i in 2:(base::length(forcecfs))){
        cash <- dplyr::bind_rows(cash,  tables[[forcecfs[i]]])
      }
    }
    cash <- cash |>
      dplyr::mutate_all(function(x) base::trimws(base::gsub("[$]","",x)))
    contain_info <- base::apply(cash, 2, collectR::detect_info_in_col)
    if (base::sum(contain_info) > 2){
      cash <- cash[, base::as.logical(contain_info)] |>
        dplyr::mutate(statement = "CFS")
      cash <- cash |>
        dplyr::filter(V1 != "") |>
        dplyr::select(statement, account = V1, dplyr::everything()) |>
        safe_format_statement()
      
      if (!base::is.null(cash$result)){
        cash <- cash$result
      } else {
        cash <- NA
      }
      
    } else cash <- NA
    
    
    
    
    
  } else if (position_cash != ""){
    cash <- tables[[position_cash]] |>
      dplyr::mutate_all(function(x) base::trimws(base::gsub("[$]","",x)))
    contain_info <- base::apply(cash, 2, collectR::detect_info_in_col)
    if (base::sum(contain_info) > 2){
      cash <- cash[, base::as.logical(contain_info)] |>
        dplyr::mutate(statement = "CFS")
      
      cash <- cash |>
        dplyr::filter(V1 != "") |>
        dplyr::select(statement, account = V1, dplyr::everything()) |>
        safe_format_statement()
      
      if (!base::is.null(cash$result)){
        cash <- cash$result
      } else {
        cash <- NA
      }
      
    } else cash <- NA
  }
  
  statements <- base::list(
    BS = balancesheet,
    IS = income,
    CFS = cash
  )
  
  return(statements)
}
