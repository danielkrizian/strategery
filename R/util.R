
#' Some Title
#' @export
is.defined <- function(x){
  exists(deparse(substitute(x)))
}


p <- function (obj) {
  #my custom way of inspecting objects
  out <- list()
  out$class <- class(obj)
  if(xtsible(obj)) {
    if(NROW(obj)>10){
      reduced <- obj[!(is.na(obj) | obj == 0) ]
      out$head <- head(reduced)
      out$tail <- tail(reduced)
    }
    
  }
  if(is.list(obj)) {
    out$names <- ls(obj)
  }
  if(is.matrix(obj)) {
    if(NROW(obj)>20){
      out$head <- head(obj)
      out$tail <- tail(obj)
    } else {
      out$data <- obj
    }
  }
  out
}
#' Some Title
#' @export
na.skip <- function (x, FUN=NULL, ...) # maybe add a trim capability?
{ # @author Brian Peterson
  
  # DESCRIPTION:
  
  # Time series data often contains NA's, either due to missing days, 
  # noncontiguous series, or merging multiple series,
  # 
  # Some Calulcations, such as return calculations, require data that 
  # looks like a vector, and needs the output of na.omit
  # 
  # It is often convenient to apply these vector-like functions, but 
  # you still need to keep track of the structure of the oridginal data.
  
  # Inputs
  # x    the time series to apply FUN too
  # FUN	function to apply
  # ...	any additonal parameters to FUN
  
  # Outputs:
  # An xts time series that has the same index and NA's as the data 
  # passed in, after applying FUN
  
  nx <- na.omit(x)
  fx <- FUN(nx, ... = ...)
  if (is.vector(fx)) {
    result <- .xts(fx, .index(x), .indexCLASS = indexClass(x))
  }
  else {
    result <- merge(fx, .xts(, .index(x)))
  }
  return(result)
}

#' Some Title
#' @export
align.digits = function(l) {
  #   http://tolstoy.newcastle.edu.au/R/e13/help/11/04/11186.html
  # USE:
  # library(gridExtra)
  # d <- align.digits(l = c(125.3, 1.23444444, 12))
  # grid.newpage()
  # grid.table(d, parse=T, core.just="left", gpar.coretext=gpar(cex=0.5))
  
  sp <- strsplit(as.character(l), "\\.")
  chars <- sapply(sp, function(x) nchar(x)[1])
  n = max(chars) - chars
  l0 = sapply(n, function(x) paste(rep("0", x), collapse=""))
  labels = sapply(seq_along(sp), function(i) {
    point <- if(is.na(sp[[i]][2])) NULL else quote(.)
    as.expression(bquote(phantom(.(l0[i])) * .(sp[[i]][1])*.(point)*.(sp[[i]][2]) ))})
  
  return(labels)
  
}

#' Some Title
#' @export
as.percent <- function(x, decimals=0, align.dec=T) {
  # x - numeric vector
  if(align.dec) {
    x <- round(x*100,decimals)
    integers <- floor(x)
    int <- formatC(integers,format="g", width=max(nchar(integers)))
    dec <- round(x - integers,decimals)
    if(any(dec!=0)){
      afterdec <- substr(formatC(dec,format="f", width=decimals+2, zero.print=F)
                         ,2
                         ,decimals+2)
    } else afterdec <- ""
    out <- paste(int,afterdec,"%",sep="")
  } else {
    x <- round(x,decimals+2)*100
    fmt <- paste("%.",decimals,"f%%",sep="")
    out <- sprintf(fmt=fmt, x)
    width<-max(nchar(out))
    out <- formatC(out,width=width) 
  }
  return(out)
}

#' Some Title
#' @export
as.dollar <- function(x, round=F) {
  # x - numeric vector
  # TODO: 100MM notation
  if(round)
    x <- round(x,round)
#   width <- k+decimals+ifelse(decimals>0,1,0)
  out <- formatC(x,format="f", big.mark="'",drop0trailing = T)
  width<-max(nchar(out))
  out <- formatC(x,format="fg", big.mark="'",width=width, drop0trailing = T)
  out <- paste(out,"$", sep="")
  return(out)
}

#' Some Title
#' @export
capwords <- function(s, strict = FALSE) {
  # capitalize first letters
  # see ?tolower examples
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' Some Title
#' @export
roundstep <- function(x,step=1){
  x <- round(x)
  if(step==5){
    if (x%%5>2) {
      return((x%/%5+1)*5)
    }
    else {
      return((x%/%5)*5)
    }
  }
  return(x)
}

#' Some Title
#' @export
chunks <- function(x,max=10000){
  #split vector in chunks with maximum length of 10000
  i <- seq_along(x)
  split(x, ceiling(i/max))
}

