# #' Constructor for class 'returns' - time series of returns
# #' 
# #' @export
# returns <- function(x, ...) {
#   UseMethod("returns",x)
# }
# 
# returns.default <- function(x, interval) {
#   p <- prices(x, interval)
#   return(as.returns(p, interval))
# }

# #' Convert object to a class time series of returns
# #' 
# #' @param x data.table. TODO: xts
# #' @param interval character vector as one of c("monthly", "regular","MTD", NA, ...).
# #' Sets the interval property of returns object. If missing, the function will attempt
# #' to discern the interval from the x object.
# #' @export
# as.returns.default <- function(x, interval=c("days","weeks","months","years")) {
#   
#   columnsInput <- c("Name", "Date", "Value")
#   columnsOutput <- c("Name", "Date", "Value")
#   .key <- c("Name","Date")
#   
#   ans <- as.data.table(x)
#   
#   #Interval column exists
#   #Interval homogeneous - delete it, to attr
#   #interval provided
#   
#   # add Interval column, if missing
#   if("Interval" %in% colnames(x)) {
#     #extract interval from data, if homogenious
#     # see if Interval is homogeneous across the series
#     data.interval <- unique(x$Interval)
#     if(length(data.interval)==1)
#       data.interval <- as.character(data.interval)
#     else {
#       stop("Interval is heterogenious across time series. Check your data")
#       # TODO: Add support for heterogenious intervals (via retaining Interval column)
#     }
#   }
#   else data.interval <- NA
#   
#   ans[, Date := as.IDate(Date)]
#   ans <- ans[, columnsInput, with=FALSE ]  # setcolorder(ans, newNames) probably not needed
#   setnames(ans, columnsOutput)
#   
#   setkeyv(ans, .key)
#   setattr(ans, "class", c(attr(ans,"class"), "returns")) 
#   # returns should be first, but problems with RStudio data viewer
#   setattr(ans, "interval", data.interval) # c("monthly","regular","MTD")
#   
#   #ans <- structure(ans, interval=interval, class=c("returns",attr(ans,"class")))
#   
#   if(!missing(interval)) {
#     ans <- to.interval(ans, to=interval)
#   }
#   return(ans)
# }

#' Convert (irregular) returns to regular intervals of chosen periodicity
#' 
#' @param x A data.table with columns Instrument, Date, Return
#' @param fill How many missing bars to fill/roll backwards and forwards?
#'             If not FALSE, a vector of length 2 - fill=c(backwards, forwards)
#'        E.g. in case of irregularly reported weekly data, fill=c(1, 4) will 
#'        roll Mondays to preceding Sundays (weekly endpoints) and 
#'        Wednesdays to the following Sundays
#' @param to Set interval of the resulting time series.
#' @param eow.fri Mark weekly endpoints as Fridays in the output. 
#'                 Internally, weekly endpoints are by default Sundays
#'                 
#' Main use is to convert from MTD type returns to periodic returns
#' Create Price index
#' Add Base Index/Price (for MTD version)
#' Make daily timeframe
#' Compress timeframe to lower interval
#' Roll price index to the period ends.
#' Set periodicity of the resulting time series.
#' TODO: accomodate other input types than MTD
to.interval.returns <- function(
  x
  , to
  , fill=if(to=="weeks") c(1,4) else if(to=="months") c(3,3) else FALSE
  , eow.fri=T
){
  
  # required because rbindlist(list(x,bases)) drops the class info
  original.class <- attr(x,"class")
  
  if("MTD" %in% attr(x,"interval")) {
    if(to %in% c("days","weeks"))
      x[, Equity:=value.index(Value
                               , todate=endpoints(Date, on="months")
                               , na.fill=NA), by=Name]
    if(to=="months") {
      # take last record of each month
      x[,c("Year","Month"):=list(year(Date), month(Date))]
      x <- x[, .SD[.N], by=list(Name,Year,Month)]
      x[,c("Year","Month"):=list(NULL,NULL)]
      setattr(x,"interval","months")
      return(x)
    }
  }
  else
    x[, Equity:=value.index(Value, todate=FALSE, na.fill=NA), by=Name]
  #add price index base
  base <- getOption("eq.base")
  if(is.null(base)) 
    base <- 100
  bases <- x[,list("Date"=round(first(Date), "months") - 1
                   , "Value"=as.numeric(NA)
                   , "Equity"=base)
             , by=Name]
  x <- rbindlist(list(x,bases))
  setkey(x,"Name","Date")
  
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
    x <- x[tf, roll=fill[2]][, Equity:=ifelse(is.na(Equity)
                                              , x[tf, roll=-abs(fill[1])]$Equity
                                              , Equity)]
  
  # Compress data timeframe to the desired interval
  setkey(tf,"Date")
  tf.periodic <- tf[Calendar()[endpoints(Date, on=to),list(Date)]
                    , nomatch=0]
  setkey(tf.periodic,"Name","Date")
  x <- x[tf.periodic]
  
  # Add returns column
  x[, Value:=roc(Equity, ,na.pad=TRUE, base.incl=TRUE), by=Name]
  x[, Equity:=NULL]
  
  # Adjust dates to Fridays instead of Sundays
  if(eow.fri & to=="weeks")
    x[,Date:=Date - 2]
  
  setattr(x,"class",original.class)
  setattr(x, "interval", to) # c("monthly","regular","MTD")
  # required because rbindlist(list(x,bases)) drops the class info
  return(x)
}

as.prices.returns <- function(returns, interval="MTD") {
  
  returns[, Value:=value.index(Value
                          , todate=endpoints(Date, on="months")
                          , na.fill=NA), by=Name]
  returns <- returns[,list(Name,Date,Value)]
#add price index base
base <- getOption("eq.base")
if(is.null(base)) 
  base <- 100
bases <- returns[,list("Date"=round(first(Date), "months") - 1
                 , "Value"=base)
           , by=Name]
prices <- rbindlist(list(returns,bases))
setkey(prices,"Name","Date")
    return(prices)
}
