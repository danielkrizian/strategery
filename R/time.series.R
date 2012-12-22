window.Date <- function(x,start=NULL,end=NULL) {
  # method for class Date. See window.zoo
  x <- as.Date(x)
  if(!is.null(start))
    x <- x[ x>=as.Date(start)]
  if(!is.null(end))  
    x <- x[ x<=as.Date(end) ]
  return(x)
}

Align <- function(x, to, pad=na.locf) {
  # Aligns observations of the xts object with another xts object. 
  # Where observation in x is missing whereas in template exists:
  #### pad=F : do not add any fills to x. 
  #### pad=na.locf : copy previous value. Maintains template time structure
  #### pad=NA : add NA value. Maintains template time structure.
  # Input:
  #  x - xts object to adjust
  #  to - xts object to use as template
  # Output: 
  #  xts object with index same as the template xts object
  #### TODO:
  # expandLast - the compressed value is expanded starting from last bar within given period (so the weekly close/high/low is available on Friday's bar) 
  # expandFirst - the compressed value is expanded starting from first bar within given period (so the weekly open is available from Monday's bar) 
  # expandPoint - the resulting array gets not empty values only for the last bar within given period (all remaining bars are Null (empty)). 
  # mode=c('expandLast','expandPoint')
  # TODO: mode='expandPoint' - i.e. perform no fill for NAs
  # TODO: mode='expandFirst' - explore na.locf(,fromLast=TRUE). Inside PadAlign function?
  
  if(length(pad))
    x <- cbind.xts(x, to, all=c(T,T), fill=pad, retside=c(T,F))
  aligned <- cbind.xts(x, to, all=c(F,T), fill=NA, retside=c(T,F))
  return(aligned)
}

string.range <- function(...) {
  # ... list of xts objects
  # returns common window as ISO8601 string
  lxts <- list(...)
  rng <- NULL
  for (x in lxts) {
    temp <- range(index(x))
    rng <- if(length(rng)) as.Date(c(max(temp[1],rng[1]),min(rng[2],temp[2]))) else temp
  }
  return(paste(rng, collapse="::"))
}

strrange <- range.string <- function
(x, # dates in format such as 2010-01-01::2010-02-01 or 2010-01-01::2010-02-01
 format=NULL # currently not used
 ){
  # TODO: recognize "2000", "2000-11" etc.
  posix <- xts::.parseISO8601(x)
  start <- as.Date(posix[[1]])
  end <- as.Date(posix[[2]])
  return(c(start,end))
}

######## data.table #######
# require(data.table)
# from = dfx[J(start),roll=TRUE,which=TRUE] 
# to = dfx[J(end),roll=TRUE,which=TRUE] 
# dfx[from:to,sum(volume),by=price] 
# or if the time intervals can be regularized, it's easier : 
# dfx[,sum(volume),by=list(hour(datetime),price)] 
