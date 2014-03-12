
as.indicator <- function(x, ...) {
  UseMethod("as.indicator",x)
}

#' @method as.indicator data.table
#' @S3method as.indicator data.table
as.indicator.data.table <- function(x, sfl=NULL) {
  setattr(x,"class",c("indicator","data.table","data.frame"))
  return(structure(x, sfl=sfl))
}

#' @method as.indicator sfl
#' @S3method as.indicator sfl
as.indicator.sfl <- function(x) {
  if(x[[1]]==quote(`%indicator%`))
    eval(x)
  else
    eval(construct(deconstruct.sfl(x)))
}

#' @method as.indicator rule
#' @S3method as.indicator rule
as.indicator.rule <- function(x) {
  return(eval(construct(deconstruct.sfl(x$signal))))
}

is.indicator <- function(x) {
  any("indicator" %in% class(x))
}

#' Construct indicator
#' 
#' @rdname indicator
#' @export indicator
indicator <- function (formula, data)  {
  if(is.sfl(substitute(formula)) & missing(data))
    return(eval(formula))
  else {
    op <- as.name("%indicator%")
    .Data <- substitute(op(data, formula))
    structure(.Data=.Data, class=c("sfl"))
  }
}

#' @rdname %indicator%
#' @export %indicator%
`%indicator%` <- function(e1, e2) {
  op <- match.call()[[1]]
  sfl <- substitute(op(e1,e2))
  .data <- e1[, Value:=eval(substitute(e2)), by=Instrument]
  .data <- .data[, list(Instrument, Date, Value)]
  .data <- align.data.table(.data, to=OHLCV)
  return(as.indicator(.data, sfl=sfl))
}

#' @method Ops indicator
#' @S3method Ops indicator
Ops.indicator <- function(e1, e2) {
  op <- as.name(.Generic)
  if(is.numeric(e2) | is.logical(e2)) {
    expr <- substitute(op(Value, e2))
    if(as.character(op) %in% c("==", "!=", "<", "<=", ">=", ">")) {
      cl <- class(e1$Value)
      .data <- e1[,Value:=as(eval(expr), cl), by=Instrument]
      .data[,Value:=as.logical(Value)]
      # because assigning directly Value:=eval(expr) throws error: 
      # "Type of RHS ('logical') must match LHS."
    }
  }
  else if(is.indicator(e2)) {
    if(as.character(op) %in% c("&", "|")) {
      .data <- unique(setkey(rbind(e1[e2, roll = T, rollends=FALSE],
                                   e2[e1, roll = T, rollends=FALSE],
                                   use.names = TRUE),
                             Instrument, Date))
      if(as.character(op) =="&")
        .data[,Value := as.logical(Value * Value.1)]
      else
        .data[,Value := as.logical(Value + Value.1)]
      
      .data <- .data[, list(Instrument, Date, Value)]
    } else {
      .data <- unique(setkey(rbind(e1[e2, roll = T, rollends=FALSE],
                                   e2[e1, roll = T, rollends=FALSE],
                                   use.names = TRUE),
                             Instrument, Date))
      expr <- substitute(op(Value, Value.1))
      .data[,Result := eval(expr)]
      .data <- .data[, list(Instrument, Date, Result)]
      setnames(.data, "Result", "Value")
    }
  }
  
  sfl <- substitute(op(e1, e2))
  return(as.indicator(.data, sfl=sfl))
}

#' @method %AND% indicator
#' @S3method %AND% indicator
`%AND%.indicator` <- function(e1, e2) {
  .data <- unique(setkey(rbind(e1[e2, roll = T, rollends=FALSE],
                               e2[e1, roll = T, rollends=FALSE],
                               use.names = TRUE),
                         Instrument, Date))
  .data[,Value := as.logical(Value * Value.1)]
  .data <- .data[, list(Instrument, Date, Value)]
  #   Z <- sig.compress(Z)
  
  op <- as.name(.Generic)
  sfl <- substitute(op(e1, e2))
  
  return(as.indicator(.data, sfl=sfl))
}

#' @method %OR% indicator
#' @S3method %OR% indicator
`%OR%.indicator` <- function(e1, e2) {
  .data <- unique(setkey(rbind(e1[e2, roll = T, rollends=FALSE],
                               e2[e1, roll = T, rollends=FALSE],
                               use.names = TRUE),
                         Instrument, Date))
  .data[,Value := as.logical(Value + Value.1)]
  .data <- .data[, list(Instrument, Date, Value)]
  #   Z <- sig.compress(Z)
  
  op <- as.name(.Generic)
  sfl <- substitute(op(e1, e2))
  
  return(as.indicator(.data, sfl=sfl))
}

#' @method print indicator
#' @S3method print indicator
print.indicator <- function(x) {
  print(data.table(x))
}

ls_indicators <- function (envir=.GlobalEnv) {
  
  all <- ls(envir=envir)
  all[sapply(all, function(x) {
    is.indicator(eval(get(x)))}
    )]
}