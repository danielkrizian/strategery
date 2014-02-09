


# RETURNS AND PRICES ------------------------------------------------------

lagratio <- function (x, lag=1, na.pad=TRUE) {
  #TODO: check FSA package
  out <- tail(x, -lag)/head(x, -lag)
  if(na.pad)
    out <- c(rep(NA, lag), out)
  return(out)
}

to.interval <- function(x, ...) {
  UseMethod("to.interval",x)
}

#' TODO: check SPX data monthly, appears to be buggy
#'
#' @param full Align to full calendar frame? If FALSE, just filter the observations
to.interval.data.table <- function(
  x
  , to = c("days","weeks","months", "quarters", "years")
  , full=FALSE
  , fill=if(to=="weeks") c(1,4) else if(to=="months") c(3,3) else FALSE
  , business.week=T
){
  
  cl <- class(x)
  
  if(full) {
    # Create calendar timeframe (daily), start-end by Instrument
    ranges <- x[,list("From"=first(Date)
                      , "To"=last(Date)), keyby=Name]
    tf <- CJ(Name=ranges$Name
             , Date=Calendar()[,Date])[
               , key=c("Name"
                       , "Date")][ranges][Date>=From & Date<=To][,list(Name
                                                                       , Date)]
    
    # Accomodate incomplete intraperiod data. 
    # Fill missing observations by carrying observations backwards and forwards
    if(length(fill)==2)
      x <- x[tf, roll=fill[2]][, Value:=ifelse(is.na(Value)
                                               , x[tf, roll=-abs(fill[1])]$Value
                                               , Value)]
    
    # Compress data timeframe to the desired interval
    setkey(tf,"Date")
    tf.periodic <- tf[Calendar()[endpoints(Date, on=to),list(Date)]
                      , nomatch=0]
    setkey(tf.periodic,"Name","Date")
    x <- x[tf.periodic]
    
    # Adjust dates to Fridays instead of Sundays
    if(business.week & to=="weeks")
      x[,Date:=Date - 2]
    
    if(to=="quarters")
      x[,Date:=as.yearqtr(Date)]
  } else 
  {
    if(to=="weeks") {
      x <- x[is.EOW(Date, weekend=T)] # Sunday, Friday, Saturday
      x <- x[endpoints(Date, on=to)]
      x[,Date:=to.EOW(Date, business=business.week)]
    }
    
    if(to=="months") {
      x <- x[is.EOM(Date)]
      x <- x[endpoints(Date, on=to)]
    }
    
  }
  
  setattr(x,"class", cl)
  
  return(x)
}

to.interval.xts <- function(
  x
  , to = c("days","weeks","months", "quarters", "years")
  , full=FALSE
  , fill=if(to=="weeks") c(1,4) else if(to=="months") c(3,3) else FALSE
  , business.week=T
){
    if(to=="weeks") {
      x <- x[is.EOW(index(x), weekend=T)] # Sunday, Friday, Saturday
      x <- x[endpoints(x, on=to),]
      index(x) <- to.EOW(index(x), business=business.week)
    }
    
    if(to=="months") {
      x <- x[is.EOM(index(x))]
      x <- x[endpoints(index(x), on=to)]
    }
  return(x)
}


# TIME SERIES -------------------------------------------------------------

# #' Align price series and compress to the lowest common interval
# #' 
# #' 
# #' @export
# set.time.frame <- function(x, interval=c("days","weeks","months")) {
#   
#   common.start <- x[,Start:=min(Date), by=Name][,max(Start)]
#   common.end <- x[,End:=max(Date), by=Name][,min(End)]
#   unit <- substr(interval,1,nchar(interval)-1)
#   from <- ceiling_date(common.start, unit=unit)
#   to <- floor_date(common.end + 1, unit=unit)
#   dts <- seq.Date(from=from, to=to, by=paste("1", unit)) - ifelse(unit=="month", 1, 0)
#   ids <- unique(x$Name)
#   x[CJ(ids,dts),Value]
# }
# a <- set.time.frame(x,interval="weeks")
# CJ()

#' Some Title
#' 
#' method for class Date. See window.zoo
#' @export
window.Date <- function(x,start=NULL,end=NULL) {
  x <- as.Date(x)
  if(!is.null(start))
    x <- x[ x>=as.Date(start)]
  if(!is.null(end))  
    x <- x[ x<=as.Date(end) ]
  return(x)
}

align <-  function(x, ...) {
  UseMethod("align", x)
}

#' Some Title
#' 
#' @param by key to join the two tables by
#' @export
align.data.table <- function(x, to, by=key(x), na.fill=c("locf", "NA", "F")){
  # Aligns observations of the data.table object with another data.table 
  # Where observation in x is missing whereas in template exists:
  #### pad=F : do not add any fills to x. 
  #### pad=na.locf : roll previous value. Maintains template time structure
  #### pad=NA : add NA value. Maintains template time structure.
  if(!setequal(key(to),by))
    stop("align.data.table: Key in 'x' does not match key in 'to'")
  keep.cols <- names(x)
  out <- x[J(to), roll=T]
  del.cols <- setdiff(names(out), keep.cols)
  out[, eval(del.cols):=NULL]
  return(out)
}


#' Some Title
#' @export
align.xts <- function(x, to, pad=na.locf) {
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

#' Returns common window as ISO8601 string
#' @param ... list of xts objects
#' @export
string.range <- function(...) {
  lxts <- list(...)
  rng <- NULL
  for (x in lxts) {
    temp <- range(index(x))
    rng <- if(length(rng)) as.Date(c(max(temp[1],rng[1]),min(rng[2],temp[2]))) else temp
  }
  return(paste(rng, collapse="::"))
}

#' Some Title
#' 
#' TODO recognize "2000", "2000-11" etc.
#' 
#' @param x dates in format such as 2010-01-01::2010-02-01 or 2010-01-01::2010-02-01
#' @param format currently not used
#' @export
strrange <- range.ISO8601 <- function
(x, 
 format=NULL
){
  
  posix <- xts::.parseISO8601(x)
  start <- as.Date(posix[[1]])
  end <- as.Date(posix[[2]])
  return(c(start=start,end=end))
}

ViewSubset <- function(x, window="1900:2015", instruments=NULL, viewer=T) {
  ISO <- strrange(window)
  out <- x[Date>=ISO$start & Date<=ISO$end]
  
  if(!missing(instruments))
    out <- x[grep(paste(instruments, collapse="|")
                  , Name
                  , ignore.case=T)]
  if(viewer)
    View(out, paste(deparse(substitute(x)), "subset") )
  return(out)
}

##### TIME & DATE ##########

#' Some Title
#' 
#' @export
annualize <- ann.factor <- function(x) {
  if(timeBased(x)) 
    x <- periodicity(as.POSIXct(x))$scale
  switch(x,
         "yearly"=,"annually"=,"year"=,"years"=,"y"=,"Y"=1,
         "semi-annually"=,"h"=,"H"=2,
         "quarterly"=,"quarters"=,"q"=,"Q"=4,
         "bi-monthly"=,"2m"=6,
         "monthly"=,"month"=,"months"=,"m"=12,
         "weekly"=,"week"=,"weeks"=,"w"=52,
         "daily"=,"day"=,"days"=,"d"=252
  )
}

#' Some Title
#' 
#' @export
compute.raw.annual.factor = function(x) {
  round( nrow(x) / compute.nyears(x) )
}

#' Some Title
#' 
#' @export
compute.annual.factor = function(x) {
  # 252 - days, 52 - weeks, 26 - biweeks, 12-months, 6,4,3,2,1
  possible.values = c(252,52,26,13,12,6,4,3,2,1)
  index = which.min(abs( compute.raw.annual.factor(x) - possible.values ))
  round( possible.values[index] )
}

as.units.frequency <- function(p) {
  #24*60*60*31*12
  if (p < 60) {
    units <- "secs"
    scale <- "seconds"
    label <- "second"
  }
  else if (p < 3600) {
    units <- "mins"
    scale <- "minute"
    label <- "minute"
    p <- p/60L
  }
  else if (p < 86400) {
    units <- "hours"
    scale <- "hourly"
    label <- "hour"
  }
  else if (p == 86400) {
    units <- "days"
    scale <- "daily"
    label <- "day"
  }
  else if (p <= 604800) {
    units <- "weeks"
    scale <- "weekly"
    label <- "week"
  }
  else if (p <= 2678400) {
    units <- "months"
    scale <- "monthly"
    label <- "month"
  }
  else if (p <= 7948800) {
    units <- "quarters"
    scale <- "quarterly"
    label <- "quarter"
  }  
  return(units)
}

seq.end <- function(from = 1, to = 1
                    , by = c("weeks", "months", "quarters", "years")
                    , length.out=NULL, along.with=NULL, ...) {
  by <- match.arg(by)
  if(identical(by,"months"))
    from <- month
  
  seq(from=from, to=to, by=by, length.out=length.out, along.with=along.with)
}


# specify to seq.Date whether one wants 
# counting done from the beginning of each month (to get the current effect)
# or the end of each month (to easily get end of month or second day to the 
# end of month sequences, etc.) with reasonable defaults.
# (At the moment, workarounds to get end of month sequences
#  include performing the seq in chron and converting to Date or 
#  doing a seq relative to the first of the NEXT month and subtracting 
#  one to get end of month.)

periodicity <- function (x, ...) 
{
  if(NROW(x)==1){
    x <- as.POSIXct(x)
    return(
      structure(list(difftime = structure(NA, units = NA, class = "difftime"), 
                     frequency = NA, start = NA, end = NA, units = NA, 
                     scale = as.character(NA), label = NA), class = "periodicity")
    )
  }
  else
    xts::periodicity(x)
}

IDateYearMonthDay <- function(x) {
  if(is.data.table(x))
  {
    x[,Year:=year(as.IDate(Date))]
    x[,Month:=month(as.IDate(Date))]
    x[,Day:=mday(as.IDate(Date))]
    return(x)
  }
}

# #' Is the date the end of the month?
# #' @param appx.days How many days to tolerate before from last day of the month?
# #' Useful when data is collected for the last business day of the month. 
# #' Recommended maximum of 6 days.
# #' @export
# is.EOM <- function(x, appx.days=0) {
#   #require(lubridate)
#   #ceiling_date(as.POSIXct(data$Date), "month") - days(1) # takes too long
#   require(timeDate)
#   #x<- data$Date
#     x<- as.Date(timeLastDayInMonth(x)) - as.Date(x) <= appx.days
#   return(x)
# }

#' End of the Month
#' 
#' Value is logical, indicating whether the Date supplied is at the end of the 
#' month.
#' @export
is.EOM <- function(x) {
  # TODO Alternative solution xts::lastof(year,month). Compare speeds
  x==as.Date(as.yearmon(x), frac = 1)
}

#' End of the Week
#' 
#' Value is logical, indicating whether the Date supplied is at the end of the week
#' @param weekend Treat Saturday and Sunday as days ending the week?
#' If FALSE, week ends on Friday
#' @export
is.EOW <- function(x, weekend=T) {
  wd <- 6 #Friday 
  if(weekend)
    wd <-c(wd, 7,1) # Saturday, Sunday
  wday(x) %in% wd
}

#' Shift date to the nearest end of the week
#' 
#' Saturdays and Sundays are shifted to the preceding Friday.
#' Value is logical, indicating whether the Date supplied is at the end of the week
#' @param business If TRUE, week ends on Friday, else it ends on Sunday
#' If FALSE, week ends on Friday
#' @export
to.EOW <- function(x, business=T) {
  wd <- wday(x)
  x[wd>1] <- x[wd>1] + 7 - wd[wd>1] + 1 # Shift non-Sundays to the following Sunday
  # Sunday=1, Saturday=7
  if(business)
    x <- x - 2
  return(x)
}

week <- function(x, starts=c("Monday", "Sunday")) {
  starts <- match.arg(starts)
  if(identical(starts,"Monday"))
    return( as.integer(strftime(as.Date(x), format="%W") ))
  else
    return( data.table::week(x))
}


getTimedate <- function(x) {
  if(is.xts(x))
    return(index(x))
  
  if(is.data.table(x))
    if(any("Date" %in% colnames(x)))
      return(x[,list(as.Date(Date))])
}

# MISSING OBSERVATIONS ----------------------------------------------------------

#' Some Title
#' 
#' Time series data often contains NA's, either due to missing days, 
#' noncontiguous series, or merging multiple series,
# 
#' Some Calculations, such as return calculations, require data that 
#' looks like a vector, and needs the output of na.omit
# 
#' It is often convenient to apply these vector-like functions, but 
#' you still need to keep track of the structure of the original data.
#' 
#' Maybe add a trim capability?
#' Inputs
#' x    the time series to apply FUN too
#' FUN  function to apply
#' ...  any additonal parameters to FUN

#' Outputs:
#' An xts time series that has the same index and NA's as the data 
#' passed in, after applying FUN
#' @author Brian Peterson
#' @export
# na.skip <- function (x, FUN=NULL, ...) 
# {   
#   
#   
#   nx <- na.omit(x)
#   fx <- FUN(nx, ... = ...)
#   if (is.vector(fx)) {
#     result <- .xts(fx, .index(x), .indexCLASS = indexClass(x))
#   }
#   else {
#     result <- merge(fx, .xts(, .index(x)))
#   }
#   return(result)
# }
