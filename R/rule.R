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

needCheckState <- function(x) {
  out = sapply(x,FUN = 
                 function(y) {
                   if(mode(y)=="call") {
                     if(as.character(y[[1]]) %in% c("Equity"))
                       return(TRUE)
                     else
                       needCheckState(y)
                   } else if(mode(y)=="name") {
                     if(as.character(y) %in% c("Equity"))
                       return(TRUE)
                   }
                 }, simplify=TRUE, USE.NAMES=F)
  names(out) = NULL
  any(unlist(out)==TRUE)
}

signalDirection <- function(size.expr) {
  # search size expression to determine direction (1, -1, 0)
  if(try(eval(eval(size.expr)), silent=TRUE)==0) {
    signal.value = 0
  } else if(identical(try(as.character(as.list(size.expr)[[1]]), 
                          silent=TRUE),"-")){
    signal.value = -1
  } else if(identical(try(as.character(as.list(as.list(as.list(size.expr)[[2]])[[2]])[[1]]), 
                          silent=TRUE), "-")){
    signal.value = -1
  } else {
    signal.value = 1
  }
  return(signal.value)
}

rule <- function(signal, size, action, check.state) {
  signal.value= switch(action,
                       "position"=,"enter"=signalDirection(size),
                       "exit"=0,
                       "rebalance"=size,
                       "order"=NULL)
  structure(.Data=list(signal=signal, signal.value=signal.value, 
                       size=size, 
                       action=action, check.state=check.state),
            class="rule")
}

#' Position
#' 
#' Difference %position% vs. %rebalance%: rebalance doesn't act if the zero position 
#' would have been crossed by the transaction or if the last position was zero. 
#' `position` is agnostic to switching sides among long/neutral/short. Rebalance isn't.
#' `allocation` is synonym to `position`
#' @export
`%position%` <- function(signal, size) {
  rule(signal, substitute(size), "position", 
       check.state=needCheckState(match.call()))
}

#' @export
`%order%` <- function(signal, size) {
  rule(signal, substitute(size), "order", 
       check.state=needCheckState(match.call()))
}

#' @export
`%enter%` <- function(signal, size) {
  rule(signal, substitute(size), "enter", 
       check.state=needCheckState(match.call()))
}

#' @export
`%exit%` <- function(signal, size) {
  rule(signal, substitute(size), "exit", 
       check.state=needCheckState(match.call()))
}

`%allocation%` <- function(signal, size) {
  rule(signal, substitute(size), "rebalance", 
       check.state=needCheckState(match.call()))
}

`%rebalance%` <- function(signal, target) {}
`%buy%` <- function(){}
`%sell%` <- function(){}
`%short%` <- function(){}
`%cover%` <- function(){}
