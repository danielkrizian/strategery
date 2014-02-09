calc <- function(x, with.name=FALSE) {
  
  if(with.name) {
    name <- as.list(match.call())[[2]]
    NextMethod("calc",x, name=name)
  } else
    UseMethod("calc",x)

}

#' Define indicator formula for later evaluation
#' 
#a <- as.list(substitute(call))
#   .f <- substitute(call)
#   fun <- as.character(.f)[1]
#   params <- names(as.list(.f))[-1]
#   params <- params[!(params %in% "")]
#   params <- as.list(.f)[-1]
#   as.character(params) %in% colnames(data)
#   params <- args[!(args %in% colnames(data))]
#   params <- lapply(ar)
#' @export
indicator <- function (call, input)  {
  .Data <- list()
  .Data$input <- deparse(substitute(input))
  .Data$call <- substitute(call)
  structure(.Data=.Data, class=c("indicator"))#,class(get("data"))))
}

ls_indicators <- function (envir=.GlobalEnv) {
  all <- ls(envir=envir)
  all[sapply(all, function(x) class(get(x))[1] == "indicator")]
}

calc.indicator <- function(x, name, ...) {
  # TODO Delete setkey line if you have installed data.table rev 999 and up
  # TODO: remove 'ohlc' dependency
  .call <- x$call
  .call <- construct(deconstruct_and_eval2(.call))
  .data <- get(x$input)[, Value:=eval(.call), by=Instrument]
  .data <- .data[, list(Instrument, Date, Value)]
  setkey(.data, Instrument, Date) # loses key, so setkey again. Bug in data.table
  .data <- align.data.table(.data, to=OHLCV)
  if(!missing(name)) {
    if(is.symbol(name)) name <- deparse(name)
    setnames(.data, "Value", name )
  }
  x$data <- .data
  return(x)
}

dat <-  function(x, ...) {
  UseMethod("data", x)
}

dat.indicator <- function(x, name) {
  calc.indicator(x, name=name)$data
}


`Ops.indicator` <- function(x, y) {
  op <- as.name(.Generic)
  sig.call <- substitute(op(x, y))
  ind <- calc.indicator(x)
  thres <- y
  expr <- substitute(op(Value, thres))
  sig.data <- ind$data[,Signal:=eval(expr), by=Instrument]
  sig.data[,Value:=NULL]
  return(signal(call = sig.call, data=sig.data))
}

print.indicator <- function(x, ...) {
  ind <- calc(x, with.name=FALSE)
  print(ind$data)
}