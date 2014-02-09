


  # ' Construct Price index from the series of returns
  # ' 
  # ' @param x numeric vector of returns, expressed as either periodic or 
  # '          period-to-date (see todate)
  # ' @param todate default FALSE for periodic x returns. If vector of integers
  # '               is supplied, these mark endpoints in x expressed as 
  # '               returns to date (e.g. MTD- month-to-date)
  # ' Warning: continuous=T, !missing(todate) not tested
# value.index <- function 
# (
#   x
#   ,todate=FALSE
#   ,continuous=F
#   ,base=getOption("eq.base")
#   ,na.fill=list(leading=NA, intra=NA, trailing=NA)
# ) {
#   add.base <- TRUE
#   if(missing(base)) {
#     add.base <- FALSE
#     base <- 100
#   }
#   if(!missing(na.fill))
#     x <- na.fill(x, fill=na.fill)
#   
#   if(!missing(todate)) {
#     R <- if(continuous)
#       c(as.numeric(NA), diff(x, lag=1))
#     else
#       lagratio(1 + x, lag=1, na.pad=TRUE) - 1 
#     R[head(todate, -1) + 1] <- x[head(todate, -1) + 1]
#   }
#   else
#     R <- x
#   
#   eq <- if(continuous) base + cumsum(R) else base * cumprod(1 + R)
#   if(add.base)
#     eq <- c(base, eq)
#   return(eq)
# }

#' Calculate returns as rates of change between subsequent prices
#' 
#' @param x numeric vector or xts object representing prices
#' TODO: test xts as input
price.ROC <- equity.ROC <- roc <- function 
(
  x
  , lag = 1
  , type = c("discrete", "continuous")
  , na.pad = TRUE
  , base.incl=TRUE
) {
  
  if(!base.incl & is.null(getOption("eq.base")))
    stop("Set options(eq.base=value).")
  else
    base <- getOption("eq.base")
  
  type <- match.arg(type)
  if (xtsible(x)) {
    x <- try.xts(x, error = as.matrix)
    if (type == "discrete") {
      roc <- x/lag(x, n, na.pad = na.pad) - 1
    }
    if (type == "continuous") {
      roc <- diff(log(x), lag, na.pad = na.pad)
    }
    reclass(roc, x)
  }
  else {
    # NAs <- NULL
    if(!base.incl) {
      x <- c(base, x)
      na.pad <- FALSE
    }
    if (na.pad) {
      # NAs <- rep(NA, lag - ifelse(base.incl, 0, 1) )
    }
    if (type == "discrete") {
      roc <- lagratio(x, lag=lag, na.pad=na.pad) - 1
    }
    if (type == "continuous") {
      roc <- diff(log(x), lag)
    }
    
    return(roc)
  }
}
