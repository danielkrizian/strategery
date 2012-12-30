# require(timeDate)
# require(mondate)
# require(RQuantLib)
# require(plyr)

#' Some Title
#' @export
loadIndicators <- function
(indicator,
 src,
 from=NULL,
 to=NULL,
 align.to=market$prices,
 env=.GlobalEnv,
 auto.assign=F
){
  if(src=="package")
    oI <- get(data(list=indicator))
  else
    oI <- get(load(src))
#   if(inherits(oI,"Date"))
#     oI <- window.Date(x=oI, start=from, end=to)
  if(is.xts(oI)){
    oI <- window(x=oI, start=from, end=to)
    if(is.xts(align.to))
      oI <- Align(oI, to=align.to, pad=na.locf)
  }
  if(auto.assign)
    assign(indicator, oI, pos=env)
  return(oI)
}

#' Some Title
#' @export
updateIndicators <- function(indicator, FUN, ...) {
  .env <- environment()

  old <- loadIndicators(indicator,auto.assign=F)
    
  FUN <- paste("indicator",indicator,sep=".")
  append <- do.call(FUN, args=list(...))
  from <- if(is.xts(old)) {index(last(old)) + 1} else {last(old) + 1}
    # append <- 

  if(NROW(append)) {
    saveSymbols.common(indicator, base_dir="Data/Indicators", env=parent.frame())
  }
}

#' Some Title
#' 
#' @param src character string indicating where to get the indicators data from. If 'package' string is provided, then data(,env=indicators)
#' @export
Prepare.indicators <- function(x, src=c("package"))
  {
  if(length(src)>1) src <- src[1]
  if(!exists("market"))
     stop("Market environment must be created first.")
     
  if(!exists('indicators'))
    indicators <- new.env()
  
  for (indicator in x)
    loadIndicators(indicator, 
                   from=range(index(market$prices))[1],
                   to=range(index(market$prices))[2],
                   src=src,
                   env=indicators, align.to=market$prices, auto.assign=T )
  
  return(indicators)
}

##### METHODS ###############
Indicator <- function(FUN='SlopeTS', params.by.symbol=NULL, frequency='daily', lag=0, pad.align=data$prices, label=TRUE, progress= "text", auto.assign=TRUE, ...) {
  # Calculate indicator for each symbol and merge them in a sigle xts.
  # Resulting multi-column xts will have structure with symbol in each column and indicator value in each row.
  # Different parameter values may apply to different symbol.
  # User can specify periodicity of indicator recalculation, i.e. daily, weekly monthly. Interperiod NA values will be filled with last observation carried forward.
  # Timestamps will be aligned with default price data.
  # Indicator may be lagged, simulating its delayed availability.
  # Maintains statistic in the ind.data environment what is the earliest timestamp that the Indicator has had non-NA value. This statistic indicates when the trading could possibly began and is used in backtesting to trim the data or in viewing the indicator summary
  
  # Use cases:
  #   1. given list of symbols, use custom function to collate indicator data for each symbol: indicator <- function(symbol)
  #   2. given data frame with columns as symbols, apply indicator over columns/symbols. Each column/symbol can have different parameter values.
  # Inputs:
  #   data - data to apply indicator on. It is the first argument in the indicator function. If not provided, dots are still used to pass arguments into function. 
  #   FUN - string name of the function to apply by column
  #   params.by.symbol - list of argument names and parameter values to apply by columns
  #   ... - other arguments - parameters to apply uniformly across symbols
  #   frequency - how often is the indicator updated
  #   delay - delay the indicator availability
  #   label - if character, use the string as column name of resulting xts. If logical, whether to prefix column names with name of the FUN. If FALSE, column names are just symbols, copied from pad.align 
  #   progress - progress bar
  #   auto.assign - should indicator be loaded to the ind.data environment
  
  # standardize inputs
  if (is.function(FUN)) FUN <- as.character(substitute(FUN))
  if (exists(paste("Indicator", FUN, sep = "."))) FUN <- paste("Indicator", FUN, sep = ".")
  
  
  args <- list(...)
  if(!is.null(formals(FUN)$lag)) # if FUN has also the lag argument, run it with lag argument
    args$lag <- lag
  
  # calculate indicator
  if(is.null(params.by.symbol)) {
    df <- do.call(FUN, args=args)
  } else {
    param.list <- lapply(params.by.symbol, as.list)
    param.matrix <- do.call(cbind,param.list)
    l <- mlply(.data=param.matrix, .fun=FUN, args , .progress=progress)
    df <- do.call(cbind.xts, l)
  }
  
  df <- switch(tolower(frequency),
               b=,bar=,bars=df,
               d=,day=,days=,daily={
                 if(periodicity(df)$scale=='daily') 
                   df 
                 else 
                   to.daily(df,OHLC=FALSE)
               },
               w=,week=,weeks=,weekly=weekly(df),
               m=,month=,months=,monthly=monthly(df))
  
  if(xtsible(pad.align))
    df <- PadAlign(x=df, template=pad.align)
  
  ind.data$signal.monitor <- list()
  if(is.null(formals(FUN)$lag)) {
    # some indicator functions might perform their own lag operation, e.g. sigCalendar. For those, do not perform lag again here
    
    ind.data$signal.monitor[[label]] <- last(df)
    df <- lag.xts(df, k=lag)
    
  }
  # label indicator
  if(is.character(label))
    colnames(df) <- label
  else {
    if(label)
      colnames(df) <- paste(as.character(FUN), colnames(df), sep='.')  
    else
      colnames(df) <- colnames(pad.align)
  }

  
  # trim the data for backtesting purposes. Remove price history before the indicator first begun. 
  # Maintain the first date of availability of all indicators.
  start <- as.Date(index(df[min(which(!is.na(df)))]))
  
  if(is.null(ind.data$common.window.start)) 
    ind.data$common.window.start <- start
  else 
    ind.data$common.window.start <- max(ind.data$common.window.start, start)
  
  # store result
  if(auto.assign) {
    assign(FUN, df, ind.data)
    
    if(is.null(ind.data$summary)) 
      ind.data$summary <- df
    else
      ind.data$summary <- merge(ind.data$summary, df)
  }
  
  return(df)
}


ind.summary <- function (dates="::") {
  # display all indicators from ind.data environment in a multi-column xts, starting with "common.window.start"
  
  env <- ind.data
  start <- ind.data$common.window.start
  
  out <- ind.data$summary[paste(as.character(start),'::',sep="")]
  out <- out[dates]
  
  return(out)
}

##### INDICATOR FUNs ###############


#' Calculates running quantile row-wise from matrix or xts
#' 
#' @export
Quantile <- function(x, probs){
  if(xtsible(x) & NCOL(x)>1 ){
    out<-t(apply(x, 1, quantile, probs=probs))
    return(reclass(out,x))
  } else 
    return(quantile(x, probs=probs))
}



Rank <- function(x , style=c('order', 'percentile', 'percentile.excel')){
  .data <- coredata(x)
  if(NCOL(.data)<2)
    stop("Cannot rank univariate series. Provide multivariate series to rank.")
  
  .rank <- function(x) rank(x, na.last = 'keep', ties.method="min")
  .rank.percentile <- function(x) ecdf(x)(x)
  .rank.percentile.excel <- function(x) {
    r <- rank(x, na.last = 'keep', ties.method="min")
    p <- (r-1)/(sum(!is.na(r))-1)
    return(p)
  }
  
  FUN <- switch(style[1],
                order=.rank,
                percentile=.rank.percentile,
                percentile.excel=.rank.percentile.excel)
  
  out <- t(apply( .data , 1, FUN=FUN))
  reclass(out,x)
}

momentum.OpCl <- function(x, n=0) {
  Op(x) - lag(Cl(x), n + 1) # this is a momentum measured at Open, from Open to Close
}

Indicator.SlopeTS <- function(symbol){
  # calculate slope of the futures term structure
  # used currently only for ttr.R:::ttr.TermStructure model
  symbol <- sub('.',' ',symbol,fixed=TRUE)
  root <- substr(symbol,1,2)
  front <- getSymbols(paste(root,'1','  B:00_0_N Comdty',sep=''),src='PRM',fields='Close',auto.assign=FALSE)
  back <- getSymbols(paste(root,'2','  B:00_0_N Comdty',sep=''),src='PRM',fields='Close',auto.assign=FALSE)
  ret <- front / back - 1
  colnames(ret) <- paste(root,'1', '.slope',sep='')
  return(ret)
}


# Indicator <- function(data, FUN='SlopeTS', params.apply.by.col=NULL, ..., data.freq=NULL, pad.align=data$prices, progress= "text") {
#   # collate data frame with symbol indicator in each column. each indicator can have different parameter values
#   # Use cases:
#   #   1. given list of symbols, use custom function to collate indicator data for each symbol: indicator <- function(symbol)
#   #   2. given data frame with columns as symbols, apply indicator over columns/symbols. Each column/symbol can have different parameter values.
#   # Inputs:
#   #   data - data to apply indicator on. It is the first argument in the indicator function. If not provided, dots are still used to pass arguments into function. 
#   #   FUN - string name of the function to apply by column
#   #   params.apply.by.col - list of argument names and parameter values to apply by columns
#   #   ... - other arguments to apply uniformly across columns
#   #   frequency - 
#   #   progress - progress bar
#   # 
#   
#   # 
#   if(!missing(data)) {
#     msg = try(match.fun(FUN)(coredata(data[[symbolnames[i]]]), ...), silent = TRUE)
#     if (class(msg)[1] != "try-error") {
#       out[, i] = msg
#     }
#     else {
#       cat(i, msg, "\n")
#     }
#   }
#   
#   # calculate indicator
#   
#   if(is.null(params.apply.by.col)) {
#     df <- do.call(FUN, args=list(...))
#   } else {
#     FUN <- if (exists(paste("Indicator", indicator, sep = "."))) paste("Indicator", FUN, sep = ".")
#     param.list <- lapply(params.apply.by.col, as.list)
#     param.matrix <- do.call(cbind,param.list)
#     l <- mlply(.data=param.matrix, .fun=FUN, ..., .progress=progress)
#     df <- do.call(cbind.xts, l)
#   }
#   
#   return(df)
# }
# ind <- getIndicator(indicator='ROC', list(n=1:19,x=data$prices), na.pad=TRUE)
# ind <- getIndicator(indicator='SlopeTS',list(symbol=symbols))
# head(ind)

### CALENDAR ####
timer <- function(now,to,pace) {

  now <- window(now,start=first(pace),end=last(pace))
  now <- window(now,start=first(to), end=last(to))
  remaining <- rowSums(outer(now,to,"<"))
  remaining[remaining==0] <- NA
  nextto <- to[length(to) - remaining + 1]
  
#   fromTodayToEnd <- foreach(todayChunks=chunks(today,max=3000), .combine=c) %do%
#     rowSums(outer(todayChunks,pace,"<"))
#   fromNextToEnd <- foreach(nexttoChunks=chunks(nextto,max=3000), .combine=c) %do%
#     rowSums(outer(nexttoChunks,pace,"=<"))

  require(data.table)
  nowplus1 <-now + 1 #to exclude now date from counting between
  duration <-   sapply(1:length(now),
                       FUN=function(i) {
                         sum(between(pace, nowplus1[i], nextto[i], incbounds=T))
                         })
  duration <- xts(duration, order.by=now)
  duration <- na.omit(duration)
  return(duration)
}

getCalendar <- function(from="1901-01-01", to="2015-12-31", calendar=NULL,
                        QuantLib.safe=TRUE, retclass="Date",
                        auto.assign=F, env=.GlobalEnv) {
  #   returns xts object of logical values.
  if(QuantLib.safe) # RQuantLib crashes when from < "1901-01-01"
    from <- max(from,'1901-01-01')
  
  days.count <- as.numeric(as.Date(to) - as.Date(from)) + 1
  dates <- as.Date(from)+0:(days.count - 1)
  if(retclass=="Date")
    return(dates)
  
  cal <- xts(rep(0, days.count), order.by=dates )
  colnames(cal) <- "Day"
  if(!is.null(calendar)){
    require(RQuantLib)
    cal <- cal[isBusinessDay(dates=as.Date(index(ind.data$Calendar)))]
  }
  
  if (auto.assign) {
    assign(  paste("calendar",calendar,sep=ifelse(is.null(calendar),"",".")) , cal , env)
    return(env)
  }
  return(cal)
}

sigCalendar <- function(dates, business.calendar=FALSE, lag=0, expand=0:0 ) {
  # dates - vector of dates (use as.Date)
  #if business calendar is provided, move by business days, otherwise by calendar days.
  # 
  shift <- lag + expand
  if(!is.character(business.calendar)) { # calendar, use index
    dates.sig <- dates + shift
  } else {  # business, use advance
    require(RQuantLib)
    dates <- dates [dates > as.Date("1901-01-01") - first(shift)+5] #QuantLib crashes under 1901-01-01
    dates.sig <- sort(as.Date(mapply(my.advance, n=shift, MoreArgs=list(calendar=business.calendar, dates=dates, timeUnit=0))))    
  }
  cal <- ind.data$Calendar
  cal[] <- 0
  cal[dates.sig] <- 1
  return(cal)
}

my.advance <- function(calendar, n, dates, timeUnit ) {
  
  if(calendar=="UnitedStates/NYSE") {
    cal <- as.Date(index(ind.data$Calendar))
    hol <- ind.data$holidaysNYSE
    we <- cal[isWeekend(calendar=calendar, dates=cal)]
    bus <- cal[!(cal %in% we)]
    bus <- bus[!(bus %in% hol)]
    advance.single <- function(date, n) {
      if(n<=0) ret <- bus[1 + sum( date > bus) + n]
      if(n>0) ret <- bus[length(bus) - sum( date < bus) + n]
#       if(n==0) stop("n=0. Use adjust instead of advance")
      ret
    }
    # 
    #   date <- as.Date("1930-10-13")
    #     dates=c("1930-10-14","1930-10-15")
    #     "1930-10-08" "1930-10-09" "1930-10-10" "1930-10-14" "1930-10-15" "1930-10-16"  "1930-10-17"
    ret <- lapply(dates, advance.single, n=n)
    ret <- as.Date(unlist(ret))
  } else 
    ret <- advance(calendar=calendar, dates=dates, timeUnit=timeUnit, n=n)
  return(ret)
}

is.CalendarPattern.old <- function(calFUN, all.dates=NULL, calendar="UnitedStates/NYSE", from="1901-01-01", to="2015-12-31", advance.days=1, business.days.only=TRUE, ...) {
  # Returns logical xts object with highlighting selected dates.
  # Input can be either date sequence to apply pattern on, or xts object.
  # Wrapper for converting irregular dates into regular time series.
  # calFUN - function for extracting specific dates according to custom filter/pattern. See ToM, Holiday, FOMC
  # advance.days
  # next.day - look forward one day?
  # x - xts to apply pattern on, optional
  #   if(next.day)
  #     to=as.character(as.Date(to) + 1) # we don't want NA when looking forward one day and preserve intended from-to window; for na.pad=FALSE
  
  args <- list(...)
  args$from <- from
  args$to <- to
  if(is.null(formals(calFUN)$calendar)) args$calendar <- NULL
  
  # all.dates
  if(is.null(all.dates)) {
    all.dates <- as.Date(as.character(timeSequence(from=from,to=to)))
    if(business.days.only) {
      all.dates <- all.dates[isBusinessDay(calendar=calendar, dates=all.dates)]
    } 
  } else {
    all.dates <- as.Date(index(all.dates))
  }
  
  # select.dates
  select.dates <- do.call(calFUN,args=args)   # select.dates <- calFUN(..., from=from, to=to)
  if(advance.days!=0) {
    select.dates <- select.dates[(as.Date(select.dates) - 5)>'1901-01-01']  # RQuantLib crashes when from < "1901-01-01"
    select.dates <- advance(calendar=calendar, dates=as.Date(select.dates), n=-advance.days, timeUnit=0)
  }
  
  ret <- xts(all.dates %in% as.Date(select.dates), order.by=all.dates)
  colnames(ret) <- as.character(substitute(calFUN))
  return(ret)
}

isDay <- function(x=ind.data$Calendar, select.dates, ..., business.adjust=TRUE, bdc=1000 , move=0, expand=0:0, move.type=c("actual", "business", "calendar"), expand.type=c("actual", "business", "calendar"), calendar="UnitedStates/NYSE", name=NULL){
  # returns xts with selected days highlighted with ones. Other days are zeroes.
  # adjust - The adjust function from RQuantLib evaluates the given dates in the context of the given calendar, and returns a vector that adjusts each input dates to the appropriate near business day with respect to the given convention.
  # bdc - business day convention; 0= Following, 1=ModifiedFollowing, 2=Preceding, 3=ModifiedPreceding. see http://en.wikipedia.org/wiki/Date_rolling
  # x will be extended to the future with business calendar of choice.
  # x - xts. Can be a full calendar object, e.g. ind.data$Calendar
  #select.dates - array of dates or FUN
  # other args passed to FUN. can be lag/advance, expand, etc.
  # TODO: simplify !ind.data$Calendar
  # TODO: support for calendar and business day shifts
  # TODO: RQuantLib crashes when from < "1901-01-01"  #   idx.sel <- idx.sel[idx.sel < NROW(x)] # RQuantLib crash fix: cap to data range
  # TODO: calendar=NULL means extend x with non-business calendar
  
  x[] <- 0
  index(x) <- as.Date(index(x))
  
  
  # extend x with full calendar
  fill.cal <- ind.data[[paste('Calendar',calendar, sep=".")]]   # find full calendar
  x <- merge(x, fill.cal[paste(range(index(x))[2] + 1, "::", sep="")], retside=c(TRUE,FALSE), fill=0)
  
  # select.dates
  if(is.character(select.dates)){
    if(existsFunction(select.dates)) {
      .FUN <- select.dates
      args <- list(...)
      select.dates <- do.call(.FUN, args)
      if(is.null(name))
        name <- .FUN
    }
  }
  select.dates <- as.Date(select.dates)
  idx.sel <- x[select.dates, drop=TRUE, which.i=TRUE] # points to expand, as indices
  
  # apply date shift
  if (move != 0 | any(expand!=0)) {
    chg <- move + expand
    idx.sel <- sort(unique(unlist(lapply(chg, function(c) idx.sel + c))))
  }
  
  x[idx.sel] <- 1
  
  #   if (move != 0 | any(expand!=0)) {
  #     move.type <- move.type[1]
  #     expand.type <- expand.type[1]
  #     select.dates <- switch(move.type, 
  #                            "business"=advance(calendar=calendar, dates=select.dates, n=move, timeUnit=0),
  #                            "calendar"=select.dates + move)
  #     select.dates <- switch(expand.type, 
  #                            "business"=sort(as.Date(mapply(advance, 
  #                                                           n=expand, 
  #                                                           MoreArgs=list(calendar=calendar, dates=select.dates, timeUnit=0)))),
  #                            "calendar"=select.dates + expand)
  #   }
  
  colnames(x) <- name
  return(x)
}

getHolidaysNYSE <- function(auto.assign=TRUE, env=.GlobalEnv) {
  # dates indicating regular NYSE holidays (ex-weekends) need to add some of the missing dates to the holidayNYSE() function. Find which ones with following code
  # http://www.arc.id.au/Calendar.html#
  # http://www.chronos-st.org/NYSE_Observed_Holidays-1885-Present.html
  
  findMissingDates <- function(){
    require(timeDate)
    require(RQuantLib)
    SPX <- getSymbols('SPX Index', src='PRM', env=.GlobalEnv, auto.assign=FALSE)
    trad <- as.Date(index(SPX)) # get actual trading days
    all <- as.Date(index(getCalendar(from=as.character(first(index(SPX))), to=as.character(last(index(SPX))), auto.assign=FALSE)))
    W <- all[isWeekend(calendar=calendar,dates=all)]
    H1 <- getHolidayList(calendar=calendar, from=as.Date(first(index(SPX))), to=as.Date(last(index(SPX))), includeWeekends=0)
    H2 <- as.Date(holidayNYSE(year=1928:2012))
    H.1and2 <- as.Date(intersect(H1, H2))
    H.1not2 <- H1[!(H1 %in% H2)] # conclusion: RQuantLib wrong 130 dates (there were trading in reality)
    H.2not1 <- H2[!(H2 %in% H1)] # use only timeDate holidays
    # H.est <- unique(sort(c(H1,H2))) # union not used due to some wrong RQuantLib dates
    H.est <- H2
    WHI <- all[!(all %in% trad)]
    HI <- WHI[!(WHI %in% W)]
    I <- HI[!(HI %in% H.est)]
    
    # manually check which were really events in http://www.chronos-st.org/NYSE_Observed_Holidays-1885-Present.html
    I.SPXnotFUN <- as.Date(c("1928-11-07","1929-11-01", "1933-03-14", "1940-11-21", "1941-11-20", "1945-08-15", "1945-08-16", "1960-11-25", "1963-11-25", "1968-04-09", "1968-06-12", "1968-06-19", "1968-06-26", "1968-07-10", "1968-07-17", "1968-07-24", "1968-07-31", "1968-08-07", "1968-08-14", "1968-08-21", "1968-08-28", "1968-09-11", "1968-09-18", "1968-09-25", "1968-10-02", "1968-10-09", "1968-10-16", "1968-10-23", "1968-10-30", "1968-11-11", "1968-11-20", "1968-12-04", "1968-12-11", "1968-12-18", "1969-02-10", "1969-03-31", "1969-07-21", "1972-12-28",  "1972-12-28", "1973-01-25", "1974-10-08", "1977-07-14", "1985-09-27", "1994-04-27", "2001-09-11", "2001-09-12", "2001-09-13", "2001-09-14", "2004-06-11", "2007-01-02")) # Victory over Japan, Presidential Funeral, Day of Mourning - Martin Luther King, Paper Crisis of 1968, Weather - Snow, Funeral Eisenhower, First Lunar Landing, Funeral Truman, Funeral Johnson, NY Blackout, Hurricane Gloria, Funeral Nixon, WTC, Funeral Reagan, Mourning Ford
    
    I[!(I %in% I.SPXnotH1)]
  }
  # Holidays missing in holidayNYSE() :
  # "Columbus Day", Thanksgiving, Christmas Eve, Day After Christmas, Day before Decoration Day in 1961, Lincoln's Birthday, Day After Independence Day,
  hol.to.add <- as.Date(c("1928-10-12","1928-11-29", "1929-11-29", "1932-10-12", "1933-03-06", "1933-03-07", "1933-03-08", "1933-03-09", "1933-03-10", "1933-03-13", "1933-10-12", "1933-11-30", "1934-10-12", "1934-11-29", "1937-10-12", "1938-10-12", "1939-10-12", "1943-10-12", "1944-10-12", "1945-10-12", "1945-12-24", "1948-10-12", "1949-10-12", "1950-10-12", "1951-10-12", "1954-12-24", "1956-12-24", "1958-12-26", "1961-05-29", "1968-02-12", "1968-07-05"))
  
  require(timeDate)
  cal <- ind.data$'Calendar'
  holidays <- as.Date(as.character(holidayNYSE(year=(as.POSIXlt(first(index(cal)))$year+1900):(as.POSIXlt(last(index(cal)))$year+1900))))
  holidays <- sort(c(holidays, hol.to.add))
  if (auto.assign) {
    assign( "holidays.NYSE" , holidays , env)
    return(env)
  }
  return(holidays)
}

#' Days around the end and the beginning of the month
#' 
#' eomNYSE is stored as index of monthly endpoints of bdNYSE
#' eomNYSE <- which(diff(as.numeric(format(bdNYSE, format = "%m")))!=0)
#' @param lag integer, indicating whether to lag the date (positive integer), or advance the date (negative integer) by (lag) number of business days
#' Special case: (0,0) is converted to (0,1)
#' @export
TurnMonth <- function(first.n.bds, last.n.bds, lag=0){
  this.env <- new.env()
  if(!is.defined(indicators$eomNYSE)) indicators$eomNYSE <- get(data(eomNYSE, envir=this.env),pos=this.env)
  if(!is.defined(indicators$bdNYSE)) indicators$bdNYSE <- get(data(bdNYSE, envir=this.env),pos=this.env)
  
  if(last.n.bds==0)
    if(first.n.bds==0)
      last.n.bds <- 1
      
  tom <- sort(as.vector(outer(indicators$eomNYSE, (-last.n.bds+1):(first.n.bds) + lag, "+")))
  tom <- indicators$bdNYSE[tom[tom > 0 & tom <= length(indicators$bdNYSE)]]
  return(tom)
}

#   tom <- sort(as.vector(outer(indicators$eomNYSE, (-last.n.bds+1):(first.n.bds)-1, "+")))
#   tom <- indicators$bdNYSE[tom[tom > 0 & tom <= length(indicators$bdNYSE)]]

ToM <- function (calendar="UnitedStates/NYSE", last.n.bds=1, first.n.bds=3) {
  # returns business days around month end
  require(RQuantLib)
  require(mondate)
  cal <- ind.data$'Calendar'
  business.eom <- unique(getEndOfMonth(calendar=calendar,dates=index(cal)))
  
  # apply function advance on multiple bd shifts around last bd of month
  ToM.dates <- sort(as.Date(mapply(advance, 
                                   n=-(last.n.bds-1):first.n.bds, 
                                   MoreArgs=list(calendar=calendar, dates=business.eom, timeUnit=0))))
  
  cal[] <- FALSE
  cal[ToM.dates] <- TRUE
  return(cal)
}


FOMCdates <- function () {
  # TODO: if meeting across consecutive days, take the last day only
  # TODO: check data lag in the database
  # TODO: implement from-to contraint
  cal <- ind.data$'Calendar'
  data <- ind.data$'FOMC'
  FOMC.dates <- as.Date(index(data))
  return(FOMC.dates)
}


###### MISC functions ######

runLength <- function(x) {
  (x) * unlist(lapply(rle(as.vector(x))$lengths, seq_len))
}

BarsSince <- function(x) runLength(!x)


ExRem <- function(x,y=!x) {
  #   removes excessive signals:
  # returns 1 on the first occurence of "true" signal in x
  # then returns 0 until y is true even if there are "true" signals in x
  filter=FALSE
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  
  for (i in 1:length(x)) {
    if(filter) {
      if(x[i]) x[i] <- FALSE
      if(y[i]) filter <- FALSE
    }
    if(x[i]) filter <- TRUE
  }
  x
}


fill <- function(x,y=!x) {
  #   works as a flip/flop device or "latch" (electronic/electric engineers will know what I mean)
  #   returns 1 from the first occurence of TRUE signal in x
  #   until a TRUE occurs in y which resets the state back to zero
  #   unil next TRUE is detected in x...  
  #   this essentially reverts the process of ExRem - multiple signals are back again
  #   TEST : fill(c(1,1,0,1),c(1,0,0,0))
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  latch <- FALSE
  for (i in 1:length(x)) {
    if(x[i]) latch <- TRUE
    if(latch) x[i] <- TRUE
    if(y[i]) latch <- FALSE
  }
  x
}

sigDuration <- function (label, data = mktdata, column , bars = 0, relationship = c("gt","lt", "eq", "gte", "lte") ) {
  
  relationship = relationship[1]
  ret_sig = NULL
  
  colNum <- match.names(column, colnames(data))
  
  sincetrue <- BarsSince (data[, colNum])
  
  
  switch(relationship, `>` = , gt = {
    ret_sig = sincetrue > bars
  }, `<` = , lt = {
    ret_sig = sincetrue < bars
  }, eq = {
    ret_sig = sincetrue == bars
  }, gte = , gteq = , ge = {
    ret_sig = sincetrue >= bars
  }, lte = , lteq = , le = {
    ret_sig = sincetrue <= bars
  })
  
  colnames(ret_sig) <- label
  return(ret_sig)
}

##### BACKUP OBSOLETE FUNS #######
is.CalendarPattern.old <- function(calFUN, all.dates=NULL, calendar="UnitedStates/NYSE", from="1901-01-01", to="2015-12-31", advance.days=1, business.days.only=TRUE, ...) {
  # Returns logical xts object with highlighting selected dates.
  # Input can be either date sequence to apply pattern on, or xts object.
  # Wrapper for converting irregular dates into regular time series.
  # calFUN - function for extracting specific dates according to custom filter/pattern. See ToM, Holiday, FOMC
  # advance.days
  # next.day - look forward one day?
  # x - xts to apply pattern on, optional
  #   if(next.day)
  #     to=as.character(as.Date(to) + 1) # we don't want NA when looking forward one day and preserve intended from-to window; for na.pad=FALSE
  
  args <- list(...)
  args$from <- from
  args$to <- to
  if(is.null(formals(calFUN)$calendar)) args$calendar <- NULL
  
  # all.dates
  if(is.null(all.dates)) {
    all.dates <- as.Date(as.character(timeSequence(from=from,to=to)))
    if(business.days.only) {
      all.dates <- all.dates[isBusinessDay(calendar=calendar, dates=all.dates)]
    } 
  } else {
    all.dates <- as.Date(index(all.dates))
  }
  
  # select.dates
  select.dates <- do.call(calFUN,args=args)   # select.dates <- calFUN(..., from=from, to=to)
  if(advance.days!=0) {
    select.dates <- select.dates[(as.Date(select.dates) - 5)>'1901-01-01']  # RQuantLib crashes when from < "1901-01-01"
    select.dates <- advance(calendar=calendar, dates=as.Date(select.dates), n=-advance.days, timeUnit=0)
  }
  
  ret <- xts(all.dates %in% as.Date(select.dates), order.by=all.dates)
  colnames(ret) <- as.character(substitute(calFUN))
  return(ret)
}



ToM.old <- function (calendar="UnitedStates/NYSE", last.n.bds=1, first.n.bds=3, from="1901-01-01", to="2015-12-31") {
  # returns business days around month end
  require(RQuantLib)
  require(mondate)
  from.adj <- as.character(mondate(from) - 1) # include previous month also, so as to have also the the first ToM bordering with the start of the from-to range
  from.adj <- max(from.adj,'1901-01-01')  # RQuantLib crashes when from < "1901-01-01"
  business.eom <- unique(getEndOfMonth(calendar=calendar,dates=as.Date(as.character(timeSequence(from=from.adj,to=to)))))
  # apply function advance on multiple bd shifts around last bd of month
  ToM.dates <- sort(as.Date(mapply(advance, n=-(last.n.bds-1):first.n.bds, MoreArgs=list(calendar=calendar, dates=business.eom, timeUnit=0))))
  ToM.dates <- as.Date(as.character(window(as.timeDate(ToM.dates), start=from, end=to))) # because advance function could have extended beyond from-to range
  return(ToM.dates)
}

Holiday.old <- function (calendar="UnitedStates/NYSE", includeWeekends=1, from="1901-01-01", to="2015-12-31") {
  # returns holiday days for given country/market calendar. Optionally, weekend holidays can be included
  # RQuantLib counts ordinary Saturday/Sunday as a Holiday. Need to exclude them via below code
  require(RQuantLib)
  from <- max(from,'1901-01-01')  # RQuantLib crashes when from < "1901-01-01"
  holiday.dates.weekday <- getHolidayList(calendar=calendar, from=as.Date(from), to=as.Date(to), includeWeekends=0)
  
  #   if(includeWeekends) {
  #     holiday.dates.weekendday <- as.Date(as.character(timeCalendar(y = as.integer(as.POSIXlt(from)$year:as.POSIXlt(to)$year+1900), m=12, d=24)))
  #   }
  #   else 
  holiday.dates.weekendday <- NULL
  holiday.dates <- sort(unique(c(holiday.dates.weekday, holiday.dates.weekendday)))
  holiday.dates <- as.Date(as.character(window(as.timeDate(holiday.dates), start=from, end=to)))
  return(holiday.dates)
}

FOMCdays.old <- function (from="1901-01-01", to="2015-12-31") {
  # TODO: if meeting across consecutive days, take the last day only
  # TODO: check data lag in the database
  # TODO: implement from-to contraint
  FOMC.dates <- as.Date(index(getSymbols("FOMC", src='PRM', fields="Close", auto.assign=FALSE)))
  return(FOMC.dates)
}
