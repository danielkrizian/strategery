#' Some text
#' 
#' @rdname %AND%
#' @export %AND%
`%AND%` <- function(x, ...) {
  UseMethod("%AND%",x)
}

#' Some text
#' 
#' @rdname %OR%
#' @export %OR%
`%OR%` <- function(x, ...) {
  UseMethod("%OR%",x)
}

#' Some text
#' 
#' @rdname %crossover%
#' @export %crossover%
`%crossover%` <- function(x, ...) {
  UseMethod("%crossover%",x)
}

`%crossover%.default` <- function(x, y) {
  c(FALSE, diff.default(x > y, lag=1, differences=1) > 0)
}

is.sfl <- function(x){
  identical(class(x),"sfl")
}

#' @method print sfl
#' @S3method print sfl
print.sfl <- function(x) {
  sfl <- construct(deconstruct.sfl(x))
  print(sfl)
  print(eval(sfl))
}

#' @method Ops sfl
#' @S3method Ops sfl
Ops.sfl <- function(e1, e2) {
  op <- as.name(.Generic)
  .Data <- substitute(op(e1, e2))
  return(structure(.Data, class="sfl"))
}

#' @method %AND% sfl
#' @S3method %AND% sfl
`%AND%.sfl` <- function(e1, e2) {
  op <- as.name(.Generic)
  .Data <- substitute(op(e1, e2))
  return(structure(.Data, class="sfl"))
}

#' @method %OR% sfl
#' @S3method %OR% sfl
`%OR%.sfl` <- function(e1, e2) {
  op <- as.name(.Generic)
  .Data <- substitute(op(e1, e2))
  return(structure(.Data, class="sfl"))
}

#' @method %crossover% sfl
#' @S3method %crossover% sfl
`%crossover%.sfl` <- function(e1, e2) {
  op <- as.name(.Generic)
  .Data <- substitute(op(e1, e2))
  return(structure(.Data, class="sfl"))
}

eval.sfl <- function(x) {
  if(is.character(x))
    x = as.language(parse(text=x))
  eval(construct(deconstruct.sfl(x)))
}

deconstruct.sfl = function(expr, envir = parent.frame(), enclos = parent.frame()) {
  if(expr[[1]]==quote(`%indicator%`)) {
    # do not deconstruct 'Close' in OHLCV %indicator% Close
    return(expr)
  }
  lapply(expr, function(m) {
    if (is.call(m)) {
      if(m[[1]]==quote(`%indicator%`)) {
        return(m)
      }
      if (m[[1]] == quote(eval)) eval(m[[2]], envir, enclos)
      else deconstruct.sfl(m, envir, enclos)
    } else if(is.name(m)){
      if(exists(as.character(m),envir=as.environment(.GlobalEnv))) {
        # & !existsFunction(as.character(m)) # commented out because of clash
        # SMA <- indicator( SMA(Close, nsma), data=OHLCV)
        obj <- eval(m, envir=as.environment(.GlobalEnv))
        if(identical(class(obj),"sfl")) {
          m1 = obj
#           m1 <- do.call(substitute, list(eval(m)))
#           if(typeof(m1)=="language") {
            if(m1[[1]]==quote(`%indicator%`)){
              return(m1) # resolves clash Close <- indicator(Close, data=OHLCV)
          }
          else if(existsFunction(as.character(m))) 
            deconstruct.sfl(m, envir, enclos) 
          else
            return(deconstruct.sfl(m1, envir, enclos))
        } else m
      } else m
    } else m
  })
}

construct = function(l) {
  if (length(l) == 0) return(NULL)
  if (length(l) == 1) return(l)
  
  if (identical(l[[1]], quote(`function`))) return(as.call(list(l[[1]], l[[2]], construct(l[[3]]))))
  
  if (!is.list(l)) return(l)
  
  as.call(setNames(lapply(l, function(m) {
    if (length(m) == 1) m
    else construct(m)
  }), names(l)))
}
