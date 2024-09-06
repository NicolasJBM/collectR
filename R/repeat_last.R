#' @name repeat_last
#' @title Replace NA by last known value
#' @author Nicolas Mangin
#' @description Replace NA in a vector by the last known value.
#' @param x Vector.
#' @param forward Logical.
#' @param maxgap Numeric.
#' @param na.rm Logical.
#' @return Vector
#' @export


repeat_last = function(x, forward = TRUE, maxgap = Inf, na.rm = FALSE) {
  if (!forward) x = base::rev(x)           # reverse x twice if carrying backward
  ind = base::which(!base::is.na(x))             # get positions of nonmissing values
  if (base::is.na(x[1]) && !na.rm)         # if it begins with NA
    ind = c(1,ind)                 # add first pos
  rep_times = base::diff(                  # diffing the indices + length yields how often
    c(ind, base::length(x) + 1) )          # they need to be repeated
  if (maxgap < Inf) {
    exceed = rep_times - 1 > maxgap  # exceeding maxgap
    if (base::any(exceed)) {               # any exceed?
      ind = base::sort(c(ind[exceed] + 1, ind))      # add NA in gaps
      rep_times = base::diff(c(ind, base::length(x) + 1) ) # diff again
    }
  }
  x = base::rep(x[ind], times = rep_times) # repeat the values at these indices
  if (!forward) x = base::rev(x)           # second reversion
  x
}
