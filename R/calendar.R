# purpose of these functions is to construct data tables of exchange-specific 
# calendar indicators of special days like holidays, FOMC, 
# business days till End-of-Month, days since EOM
# These data tables can then be loaded and persisted in the database (SQL Server)

# Generate All calendar dates

# If retclass=xts, returns xts object named "Day", with zeroes.
makeCalendar <- function(y=1800:1915, from, to,
                        QuantLib.safe=TRUE, retclass=c("data.table", "Date", "xts"),
                        auto.assign=F, env=.GlobalEnv) {
  
  if(length(retclass)>1)
    retclass <- retclass[1]
  if(QuantLib.safe) # RQuantLib crashes when from < "1901-01-01"
    from <- max(from,'1901-01-01')
  if(missing(from))
    from <- paste(y[1],"12","31",sep="/")
  if(missing(to))
    to <- paste(y[length(y)],"12","31",sep="/")
  dates <- seq(from=as.Date(from), to=as.Date(to),by = "day")
  # see also ?seq.Date. Compare speeds.
  
  if(identical(retclass, "data.table"))
    out <- data.table(Date=as.IDate(dates), key="Date")
  
  if(identical(retclass, "Date"))
    out <- dates
  
  if(identical(retclass, "xts")) {
    out <- xts(rep(0, days.count), order.by=dates )
    colnames(out) <- "Day"
  }
  
  if (auto.assign) {
    assign( "calendar"  , out , env)
    return(env)
  }
  return(out)
}

#' Construct a calendar of all days
#' 
#' Warning: Saturdays before 1952-09-29 not yet accomodated
#' @source NYSE Holidays: http://www.nyse.com/pdfs/closings.pdf
#' @source NYSE Trading Hours (Saturdays before 1952-09-29): http://www.nyse.com/pdfs/historical_trading_hours.pdf
#' @export
Days.Calendar <- function (y=1800:2015) {
  return(makeCalendar(y=y, QuantLib.safe=FALSE, retclass="data.table"))
}

#' Construct an Exchange-specific calendar of trading (business) days
#' 
#' Warning: Saturdays before 1952-09-29 not yet accomodated
#' @source NYSE Holidays: http://www.nyse.com/pdfs/closings.pdf
#' @source NYSE Trading Hours (Saturdays before 1952-09-29): http://www.nyse.com/pdfs/historical_trading_hours.pdf
#' @export
Days.Trading <- function (exchange="NYSE") {

  if(identical(exchange,"NYSE")) {
    zone <- "NewYork"
    FinCenter <- "NewYork"
    holidayFUN <- "holidayNYSE"
  }
  y <- 1800:2015
  
  
  cal <- makeCalendar(y=y, QuantLib.safe=FALSE, retclass="data.table")
  cal[,wday:=as.POSIXlt(Date)$wday] #day of the week
  cal[,exchange:=exchange]
  hol <- data.table(Date=as.IDate(as.Date(as.character( do.call( holidayFUN, list(y)))))
                    ,key="Date")
  
  cal[,isHoliday:=FALSE]
  cal[hol, isHoliday:=TRUE]
  cal[,isBizday:=isBizday ( as.timeDate(as.Date(Date)
                                        , zone = zone
                                        , FinCenter = FinCenter)
             , holidays = do.call( holidayFUN, list(y)) 
                            
             , wday = 1:5)] 
  cal[, nth.bday.of.month:=cumsum(isBizday)
      , by=list(month=round(Date, "months"))]
  cal[, last.nth.bday.of.month:=sum(isBizday) - nth.bday.of.month + 1
      , by=list(month=round(Date, "months"))]
}

