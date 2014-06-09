# TRANS -------------------------------------------------------------------

runLength <- function(x) {
  (x) * unlist(lapply(rle(as.vector(x))$lengths, seq_len))
}


#' Number of bars since a condition has been met
#' 
#' @export
BarsSince <- function(x) runLength(!x)

#' Number of bars to the next TRUE value
#' 
#' @export
BarsTo <- function(x) {
  
  rle <- rle(x)
  unlist(mapply(function(value, length)
  {
    if(value) rep(0, length) else seq(from=length, to=1)
  }
  , value=rle$values
  , length=rle$lengths))
}

#' Remove excessive signals
#' 
#' returns 1 on the first occurence of "true" signal in x
#' then returns 0 until y is true even if there are "true" signals in x
#' @export
ExRem <- function(x,y=!x) {
  filter=FALSE
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  
  for (i in 1:length(x)) {
    if(filter) {
      if(x[i]) x[i] <- FALSE
      if(y[i]) filter <- FALSE
    }
    if(x[i]) filter <- TRUE
  }
  x
}

#' Remove excessive signals
#' 
#' works as a flip/flop device or "latch" (electronic/electric engineers will know what I mean
#' returns 1 from the first occurence of TRUE signal in x
#' until a TRUE occurs in y which resets the state back to zero
#' unil next TRUE is detected in x...  
#' this essentially reverts the process of ExRem - multiple signals are back again
#' TEST : fill(c(1,1,0,1),c(1,0,0,0))
#' @export
Fill <- function(x,y=!x) {
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  latch <- FALSE
  for (i in 1:length(x)) {
    if(x[i]) latch <- TRUE
    if(y[i]) latch <- FALSE
    if(latch) x[i] <- TRUE
    #     if(y[i]) latch <- FALSE # include also this line in a variant where x=T where y=T
  }
  x
}

#' Remove excessive signals
#' 
#' Gives a "1" or true on the day that x crosses above y Otherwise the result is "0".
#' To find out when x crosses below y, use the formula Cross(y, x) 
#' @export
Cross <- function(x, y) {
  above <- x > y
  #below <- y < x
  ExRem(above)
}

#' @export
anticipate <- function(x, k=1, pad=NA) {
  k <- abs(k)
  c( tail(x, -k), rep(pad, k) )
}

#' @export
delay <- function(x, k=1, pad=NA) {
  k <- abs(k)
  c( rep(pad, k) , head(x, -k) )
}

shift <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  if(k>0) {
    return( c(rep(NA, k), x)[1 : length(x)] )
  }
  else if(k<0) {
    return( c(x[(-k+1):length(x)], rep(NA, -k)) )
  }
  else if(k==0)
    return(x)
  
}


# DATA.TABLE --------------------------------------------------------------

na.zero = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
  return(DT)
}

# get the names of the unkeyed columns of the data table.
varnames <- function(x, first=T) {
  keyed <- key(x)
  all <- colnames(x)
  selected <- all[!(all %in% keyed)]
  ifelse(first, selected[1], selected)
}

merge.roll <- function(x, y){
  k <- key(x)
  ret <- unique(setkeyv(rbind( x[y, roll = T]
                               , y[x, roll = T]
                               , use.names = TRUE)
                        , k))
  return(ret)
}





# VECTORS -----------------------------------------------------------------

# first <- function(x) x[1]
# 
# last <- function(x) x[length(x)]

# FORMATTING --------------------------------------------------------------

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
align.decimal <- function (x, dechar = ".", nint = NA, ndec = NA, pad.left = TRUE) 
{
  splitchar <- paste("[", dechar, "]", sep = "")
  splitlist <- strsplit(as.character(x), splitchar)
  ints <- unlist(lapply(splitlist, "[", 1))
  ints[is.na(ints)] <- "0"
  if (pad.left) {
    if (is.na(nint)) 
      nint <- max(nchar(ints))
    ints <- sprintf(paste("%", nint, "s", sep = ""), ints)
  }
  if (is.na(ndec)) 
    ndec <- max(nchar(unlist(lapply(splitlist, "[", 2))))
  decs <- unlist(lapply(splitlist, "[", 2))
  decs[is.na(decs)] <- "0"
  decs <- sprintf(paste("%-", ndec, "s", sep = ""), decs)
  return(paste(ints, decs, sep = dechar))
}

# t(t(format(x, digits=2,drop0trailing=T, trim=F,scientific=FALSE)))

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
  labels = sapply(seq_along(sp), function(i) 
  {
    point <- if(is.na(sp[[i]][2])) NULL else quote(.)
    as.expression(bquote(phantom(.(l0[i])) * .(sp[[i]][1])*.(point)*.(sp[[i]][2]) ))
  })
  
  return(labels)
  
}

#' Some Title
#' 
#' @source http://stackoverflow.com/questions/7145826/how-to-format-a-number-as-percentage-in-r
#' @export
# percent <- function(x, digits = 2, format = "f", ...)
# {
#   paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
# }
as.percent <- function(x, decimals=0, align.dec=T, pct.sym=F) {
  # x - numeric vector
  if(align.dec) {
    x <- round(x*100,decimals)
    integers <- floor(x)
    int <- formatC(integers,format="g", width=max(nchar(integers)))
    dec <- ifelse(is.na(integers),0,round(x - integers,decimals))
    if(any(dec!=0)){
      afterdec <- substr(formatC(dec,format="f", width=decimals+2, zero.print=F)
                         ,2
                         ,decimals+2)
    } else afterdec <- ""
    pct.sym <- ifelse(pct.sym,"%","")
    out <- paste(int,afterdec, pct.sym, sep="")
  } else {
    x <- round(x,decimals+2)*100
    pct.sym <- ifelse(pct.sym,"%%","")
    fmt <- paste("%.",decimals,"f", pct.sym , sep="")
    out <- sprintf(fmt=fmt, x)
    width<-max(nchar(out))
    out <- formatC(out,width=width) 
  }
  return(out)
}



#' Some Title
#' 
#' @export
# see also dollar() function in library(prettyR)
# dollar <- function (x) 
# {
#   x <- round_any(x, 0.01)
#   if (max(x, na.rm = TRUE) < largest_with_cents & !all(x == 
#                                                          floor(x), na.rm = TRUE)) {
#     nsmall <- 2L
#   }
#   else {
#     x <- round_any(x, 1)
#     nsmall <- 0L
#   }
#   str_c("$", format(x, nsmall = nsmall, trim = TRUE, big.mark = ",", 
#                     scientific = FALSE, digits = 1L))
# }
# <environment: 0x000000001229ee98>
#   > dollar_format
# dollar_format <- function (largest_with_cents = 1e+05) 
# {
#   function(x) {
#     x <- round_any(x, 0.01)
#     if (max(x, na.rm = TRUE) < largest_with_cents & !all(x == 
#                                                            floor(x), na.rm = TRUE)) {
#       nsmall <- 2L
#     }
#     else {
#       x <- round_any(x, 1)
#       nsmall <- 0L
#     }
#     str_c("$", format(x, nsmall = nsmall, trim = TRUE, big.mark = ",", 
#                       scientific = FALSE, digits = 1L))
#   }
# }
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

metric_format <- function(
  x
){
  #   Values Formatting
  for(row in 1:NROW(x)) {
    val <- as.numeric(x[row,])
    val<- switch(rownames(x)[row],
                 
                 #### 3.10%
                 'CAGR'=,'Annualized Return'=,
                 'Volatility'=,'Annualized StDev'=,
                 'Average Trade'=,
                 'Average Win'=,
                 'Average Loss'=,
                 'Best Trade'=,
                 'Worst Trade'=,
                 'Average Winning Month'=,   
                 'Average Losing Month'=,
                 'Best Month'=, 
                 'Worst Month'=,    
                 'Best Year'=,   
                 'Worst Year'=,
                 'Expectancy'=sprintf("% .2f%%",val*100),
                 
                 #### -0.10%
                 'Max Daily Drawdown'=,'Max Drawdown'=,'Max DD'=,
                 'Average Drawdown'=,
                 'VaR.5%'=,
                 'CVaR'=sprintf("% .2f%%",-abs(val)*100),
                 
                 #### 52%
                 'Total Return'=,
                 'Time In Market'=,'Exposure'=,
                 'Trade Winning %'=,
                 '% Winning Months'=,
                 '% Winning Years'=,
                 'Positive 12 Month Periods'=sprintf("%.0f%%",val*100),
                 
                 #### 6.26
                 'Sharpe'=,
                 'DVR'=,
                 'MAR'=,
                 'Skew'=,
                 'Kurt'=,
                 'Win/Loss Ratio'=,
                 'Profit Factor'=,
                 CAGR.MaxDD=,
                 Min.Eq=,
                 U.Capture=,
                 D.Capture=,
                 U.Number=,
                 D.Number=,
                 U.Pctge=,
                 D.Pctge=,
                 p=round(val,2),
                 
                 #### 3.1
                 'Avg Drawdown Length'=,
                 'Avg Trades Per Year'=,
                 'Avg Days In Trade'=round(val,1),
                 
                 #### 31
                 'Trades'=round(val,0),
                 
                 #### Date Range
                 'Time Period'=val,
                 
{ #default format option
  if(format=='percent') sprintf(fmt=paste("%.",digits,"f%%", sep=""), val*100)
  else round(val,digits)
})
    x[row,] <-val
  }
  
}




# TABLE FORMATTING -------------------------------------------------------------

#' numeric adjustments - round, scientific, negative, as percent
#' align decimals - always colwise - format()
#' add marks (big numbers, decimal ...) - always colwise - replace strings
#' add percent (%) sign to the end - concatenate string
#' add currency ($, GBP ...) sign to the beginning - concatenate string
#' TODO add brackets instead of negative sign - concatenate string
#' output to either graphical device, document or html(webpage)
#' 
#' "pct" - 0.26% - round(x, digits=2)
#' "ccy" - GBP 205'265'365
#' 


#' Format summary data.table or data.frame by column or by row
#' Each col/row can be formatted separately
#' 
#' Arguments support vectors
#' @param x - list, matrix or data frame of statistics. List will be converted to matrix
#' @export
format.table <- function(
  x
  , by.col=T
  , special=F # c("pct","ccy")
  , percent=F # c(T,T,T,T,T, ...)
  , round=2
  , negative=F
  #, scientific =list(bycol=F,byrow=F)
  
){  
  # data standardization

  x <- as.data.frame(x)
  col.names <- colnames(x) # coercion creates unwanted name x
  
  if(!by.col)
  x <- as.data.frame(t(x))
  
  #' numeric adjustments - round, scientific, negative, as percent
  pct <- function(x, percent) if(is.numeric(x) & percent) 100*x else x
  rnd <- function(x, digits) if(is.numeric(x)) round(x, digits) else x
  neg <- function(x, negative) if(is.numeric(x) & negative) -abs(x) else x

  x <- as.data.frame(mapply( pct , x , percent, SIMPLIFY=FALSE ))
  x <- as.data.frame(mapply( rnd , x , round, SIMPLIFY=FALSE ))
  x <- as.data.frame(mapply( neg , x , negative , SIMPLIFY=FALSE ))
  x <- as.data.frame(mapply( format , x , MoreArgs=list(digits=9
                                                        , drop0trailing=T
                                                        , trim=F
                                                        , scientific=FALSE)
                             , SIMPLIFY=FALSE ))
  if(!by.col)
    x <- as.data.table(t(x))
  
  colnames(x) <- col.names
  
  return(x)
}


#' Subset, sort, relabel, transpose a table
#' 
#' @param x - list, matrix or data frame of statistics. List will be converted to matrix
#' @param select.rows, select.cols - character vector specifying which rows/cols to only display as row names and col names 
#' @param subset - TODO. currently only select rows is enabled
#' @param metrics.in.rows - supports both tables with metrics in rows or in columns
#' @export
organize.table <- function(
  x
  , select.rows
  , select.cols
  , subset=NULL
  , sort=NULL
  , ascending=NULL
  , row.names=rownames(x)
  , col.names=colnames(x)
  , metrics.in.rows = TRUE
){
  
  # data standardization
  if (is.list(x)) 
    x <- as.data.frame(unlist(x))
  
  # Select rows and columns, TODO subset data
  if(!missing(select.rows) | !missing(select.cols)) {
    if(missing(select.rows))
      select.rows <- rownames(x)
    if(missing(select.cols))
      select.cols <- colnames(x)
    x <- subset(x, rownames(x) %in% select.rows , select.cols )
  }
  
  x <- as.data.frame(x)
  colnames(x) <- NULL # coercion creates unwanted name x
  
  # Sort
  if (length(sort) && nrow(x)>1) {
    
    if(is.character(sort)) {
      if(is.null(ascending)) ascending<-FALSE
      row <- match(sort,rownames(x))
      if(is.na(row)) row <- match(sort,rownames(applyLabels(x)))
      sort.factors<- x[row,]
    }
    else if(identical(sort,row.names)) {
      if(is.null(ascending)) ascending<-TRUE
      sort.factors <- col.names(x)
    }
    if(is.logical(ascending)) ascending<-ifelse(ascending,1,-1)
    x<- x[,order(ascending*xtfrm(sort.factors))]
  }
  
  if(!metrics.in.rows) x <- t.data.frame(x)
  
  # metric labels
  applyLabels <- function(x) {
    replace <- c(
      CAGR.MaxDD = 'CAGR/MaxDD',
      Trades.In.Year='Trades/Yr',
      Win.Trades.Pct='Win Trades'
    )
    x <- rename(x, replace=replace)
    colnames(x) <- gsub('.'," ", colnames(x),fixed=TRUE)
    x
  }
  
  if(!identical(col.names,colnames(x))) {
    x <- applyLabels(x)
    colnames(x) <- col.names
  }
  if(!identical(row.names,rownames(x))) 
    rownames(x) <- row.names
  
  return(x)
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

#' Split vector in chunks with maximum length of 10000
#' @export
chunks <- function(x,max=10000){

  i <- seq_along(x)
  split(x, ceiling(i/max))
}

# PUBLISHING --------------------------------------------------------------


knit2wp.com <- function(file) {
    require(XML)
    post.content <- readLines(file)
    post.content <- gsub(" <", "&nbsp;<", post.content)
    post.content <- gsub("> ", ">&nbsp;", post.content)
    post.content <- htmlTreeParse(post.content)
    post.content <- paste(capture.output(print(post.content$children$html$children$body, 
        indent = FALSE, tagSeparator = "")), collapse = "\n")
    post.content <- gsub("<?.body>", "", post.content)
    post.content <- gsub("<p>", "<p style=\"text-align: justify;\">", post.content)
    post.content <- gsub("<?pre><code class=\"r\">", "\\[sourcecode language=\"r\"\\]\\\n ", 
        post.content)
    post.content <- gsub("<?pre><code class=\"no-highlight\">", "\\[sourcecode\\]\\\n ", 
        post.content)
    post.content <- gsub("<?/code></pre>", "\\\n\\[/sourcecode\\]", post.content)
    return(post.content)
}

# OBJECT MANAGEMENT -------------------------------------------------------

#' Check if object is defined/exists
#' 
#' @export
is.defined <- function(x){
  exists(deparse(substitute(x)))
}

#' My custom way of inspecting objects
#' @export
p <- function (obj) {
  
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

#' Source all R files in a given folder
#'
#' Credits: Yang Feng's Blog (http://yangfeng.wordpress.com/2009/10/19/how-to-source-all-the-r-files-in-a-folder/)
#' @export
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

# PERFORMANCE BENCHMARKING -------------------------------------------------------------

#' Some Title
#' @export
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  #   Is there an R timer or stopwatch function similar to MATLAB's tic/toc?
  #   http://stackoverflow.com/questions/1716012/stopwatch-function-in-r/1716344#1716344
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

#' Some Title
#' @export
toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

#' Some Title
#' @export
my.perf <- function(expr) {
  # my.perf(quote(example(glm)))
  Rprof(tmp <- tempfile())
  eval(expr)
  Rprof()
  ret <- summaryRprof(tmp)
  unlink(tmp)
  return(ret)
}

# DEBUGGING ---------------------------------------------------------------

#' Some Title
#' 
#' read R Inferno Circle 8
#' @export
my.debug <- function() {
  options(warn=2) ### options(warn=1)
  options(error=recover) # options(error=stop)
}
# sys.call()
#' Some Title
#' 
#' @export
my.undebug <- function() {
  options(warn=1)
  options(error=stop)
}
