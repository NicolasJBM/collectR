#' @name return_statement_position
#' @title Find statements positions
#' @author Nicolas Mangin
#' @description Identify the table which is the most likely to be the desired financial statement.
#' @param tables List. all tables from a 10-K annual report.
#' @param patterns List of character vectors. Patterns identifying the statements.
#' @param remove Numeric vector. Position of the tables to be removed.
#' @param minscore Integer. Minimum number of pattern occurrences to keep a table as a candidate for a statement.
#' @param window Integer. To keep candidates having the best scores or scores near the best scores.
#' @return Numeric vector of positions
#' @export


return_statement_position <- function(tables, patterns, remove = NA, minscore = 3, window = 2){
  positions <- base::setdiff(base::names(tables), remove)
  tables <- tables[positions]
  positions <- base::names(tables)
  for (pattern in patterns){
    if (base::length(positions) > 1){
      newpositions <- positions[base::unlist(base::lapply(tables, collectR::detect_fs_pattern, pattern))]
      if (base::length(newpositions) > 0) positions <- newpositions else positions <- positions
      tables <- tables[positions]
      positions <- base::names(tables)
    }
  }
  score <- base::unlist(base::lapply(tables, collectR::score_fs_patterns, patterns))
  sufficient <- score > minscore
  bestmatches <- score > base::max(score)-window
  positions <- positions[bestmatches & sufficient]
  return(positions)
}

