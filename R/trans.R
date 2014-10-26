RoR <- pchange <- function(x, na.pad=T) {
  RoR = diff.default(x)/x[-length(x)]
  if(na.pad)
    c(NA, RoR)
  else
    RoR
}

change <- function(x, na.pad=T) {
  if(na.pad)
    c(NA, diff.default(x))
  else
    diff.default(x)
}

#' Crossover operator
#' 
#' Crossover occurs (indicates TRUE, otherwise indicates FALSE) when x value
#' "crosses over" y value - times t where x[t-1] < y[t-1] and x[t] > y[t]
#' 
#' @rdname crossover
#' @exportMethod `%crossover%`
#' @export
`%crossover%` <- function(x, ...) {
  UseMethod("%crossover%",x)
}

#' @export
`%crossover%.default` <- function(x, y) {
  c(FALSE, diff.default(x > y, lag=1, differences=1) > 0)
}