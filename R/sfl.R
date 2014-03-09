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

deconstruct.sfl = function(expr, envir = parent.frame(), enclos = parent.frame()) {
  lapply(expr, function(m) {
    if (is.call(m)) {
      if (m[[1]] == quote(eval)) eval(m[[2]], envir, enclos)
      else deconstruct.sfl(m, envir, enclos)
    } else if(is.name(m)){
      if(exists(as.character(m),envir=as.environment(.GlobalEnv)) &
           !existsFunction(as.character(m))) {
        obj <- eval(m, envir=as.environment(.GlobalEnv))
        if(identical(class(obj),"sfl")) {
          m1 <- do.call(substitute, list(eval(m)))
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
