ls_rules <- function (envir=.GlobalEnv) {
  all <- ls(envir=envir)
  all[sapply(all, function(x) is.rule(get(x)))]
}

# print.rule <- function(rule) {
#   print(eval.rule(rule)$portfolio)
# }

is.rule <- function(x) {
  inherits(x, "rule")
}

#' Position
#' 
#' Difference %position% vs. %rebalance%: rebalance doesn't act if the zero position 
#' would have been crossed by the transaction or if the last position was zero. 
#' `position` is agnostic to switching sides among long/neutral/short. Rebalance isn't.
#' `allocation` is synonym to `position`
#' @export
`%position%` <- function(signal, size) {
  structure(.Data=list(signal=signal, size=size, action="position"), class="rule")
}

#' @export
`%enter%` <- function(signal, size) {
  structure(.Data=list(signal=signal, size=size, action="enter"), class="rule")
}

#' @export
`%exit%` <- function(signal, size) {
  structure(.Data=list(signal=signal, size=size, action="exit"), class="rule")
}

`%allocation%` <- function(signal, size) {
  structure(.Data=list(signal=signal, size=size, action="enter"), class="rule")
}

`%rebalance%` <- function(signal, target) {}
`%buy%` <- function(){}
`%sell%` <- function(){}
`%short%` <- function(){}
`%cover%` <- function(){}
