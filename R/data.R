# CALENDAR ----------------------------------------------------------------

#' Generate All calendar dates
#' 
#' If retclass=xts, returns xts object named "Day", with zeroes.
#' Construct a calendar of all days
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
Calendar <- function (y=1800:2015) {
  out <- makeCalendar(y=y, QuantLib.safe=FALSE, retclass="data.table")
  out[,year:=year(Date)]
  out[,month:=month(Date)]
  out[,week:=week(Date)]
  out[,wday:=as.POSIXlt(Date)$wday] #day of the week
  out[,day:=mday(Date)]
  setkey(out,"Date")
  return(out)
}

#' Construct an Exchange-specific calendar of trading (business) days
#' 
#' Table of exchange definitions -  contains trading bars
#' Warning: Saturdays before 1952-09-29 not yet accomodated
#' @source NYSE Holidays: http://www.nyse.com/pdfs/closings.pdf
#' @source NYSE Trading Hours (Saturdays before 1952-09-29): http://www.nyse.com/pdfs/historical_trading_hours.pdf
#' @export

# calendar2 <- function(year=format(Sys.Date(), "%Y"), FUN=holidayNYSE) {
#   # the next few lines should be removed when this code is added to a package
#   # that Imports timeDate
#   if (!"package:timeDate" %in% search()) {
#     suppressPackageStartupMessages({ 
#       if (!require(timeDate)) {
#         stop("timeDate must be installed to use this function.")
#       }
#     })
#     on.exit(detach(package:timeDate, unload=TRUE))
#   }
#   ## End of code that should be removed when this is added to a package
#   year <- as.numeric(year)
#   fun <- match.fun(FUN)
#   do.call('c', lapply(year, function(y) {
#     holidays <- as.Date(fun(year=y))
#     all.days <- seq.Date(as.Date(paste(y, '01-01', sep='-')), 
#                          as.Date(paste(y, '12-31', sep='-')), by='days')
#     nohol <- all.days[!all.days %in% holidays]
#     nohol[!format(nohol, '%w') %in% c("6", "0")] #neither holiday nor weekend
#   }))
# }

calendar <- function (exchange=NULL, years=1800:2015, from, to, QuantLib.safe=TRUE) {
  
  # if(QuantLib.safe) # RQuantLib crashes when from < "1901-01-01"
  require(timeDate)
  if(missing(from))
    from <- paste(years[1],"01","01",sep="/")
  if(missing(to))
    to <- paste(years[length(years)],"12","31",sep="/")

  all.days <- seq.Date(from=as.Date(from), to=as.Date(to), by = "days")
  
  if(is.null(exchange))
    return(all.days)
  

  if(identical(exchange,"NYSE")) {
    zone <- "NewYork"
    FinCenter <- "NewYork"
    holidayFUN <- "holidayNYSE"
  }
  
    holidays <- as.Date(do.call(holidayFUN, list(year=years)))
    nohol <- all.days[!all.days %in% holidays]
    noholw <- nohol[!format(nohol, '%w') %in% c("6", "0")] 
    #neither holiday nor weekend

  return(noholw)
}




#' Construct bars frame for each symbol
#' 
#' If exchange = NULL - calendar days as opposed to trading days
#' @export
time.frame <- function(symbols, bds=TRUE) {

  if(bds){
    #Exchange, Instrument data.table
    IE <- data.table(Instrument=symbols)
    IE[,Exchange:=getInstrument(Instrument)$exchange] 
    setkey(IE,Exchange)
    
    ED <- rbindlist(lapply(unique(IE)$Exchange,
                           function(e) data.table(Exchange=e
                                                  , Date=calendar(e))))
    #Exchange, Date data.table
    setkey(ED, Exchange)
    
    ID <- IE[ED][,list(Instrument, Date)] # Instrument, Date data.table
  } else {
    ID <- CJ(Instrument=symbols, Date=calendar())
  }

  setkey(ID, Instrument, Date)
  return(ID)
}



# INSTRUMENTS -------------------------------------------------------------------
#' Convert xts to data.table
#'
#' @export
as.data.table.xts <- function(x){
  #' #http://r.789695.n4.nabble.com/data-table-and-time-series-subsetting-td4633223.html
  #' http://stackoverflow.com/questions/17345951/data-table-time-subset-vs-xts-time-subset
  DT <- as.data.table(as.data.frame(x))
  DT[, Date:=index(x)]
  setkey(DT,Date)
  setcolorder(DT,c("Date",names(x)))

  return(DT)
}

# as.assets <- function(x) {
#   # multi-assets
#   
# x <- as.data.table(x)
#     
#     DT <- as.data.table(as.data.frame(x))
#   DT[, Date:=index(x)]
#   setkey(DT,Date)
#   setcolorder(DT,c("Date",names(x)))
#   
#     is.null(attr(x,"name"))
#     match("Instrument",names(x))
#     multiasset <- 
#   xtscols <- names(x)
#   
#   measure.vars <- intersect(c("Open","High","Low","Close", "Price", "Return"))
#   
#   DT <- data.table(Date=as.IDate(index(x))
#                    , Open=op
#                    , High=hi
#                    , Low=lo
#                    , Close=cl)
#   
#   dots <- melt.data.table(data, id=c(icol), measure.vars=NULL,
#                           variable.name=c("Date"),value.name="Return",
#                           na.rm=TRUE, variable.factor=FALSE)
#   instruments <- ifelse(!is.na(icol), x[,icol], attr(x,"name")
#                         
#                         require(quantmod)
#                         if(is.OHLC(x)) {
#                           op <- as.numeric(x[,has.Op(x,which=T)])
#                           hi <- as.numeric(x[,has.Hi(x,which=T)])
#                           lo <- as.numeric(x[,has.Lo(x,which=T)])
#                           cl <- as.numeric(x[,has.Cl(x,which=T)])
#                           DT <- data.table(Date=as.IDate(index(x))
#                                            , Open=op
#                                            , High=hi
#                                            , Low=lo
#                                            , Close=cl)
#                         }
#                         
#                         
#                         name <- ifelse(,as.character(substitute(x)))
#                         
#                         
#                         setkeyv(DT, c(instrument.col,"Date"))  
#                         return(DT)
# }

has.Return <- function (x, which = FALSE) 
{
  colAttr <- attr(x, "Return")
  if (!is.null(colAttr)) 
    return(if (which) colAttr else TRUE)
  loc <- grep("return", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

getDatabasePath <- function(){
  "G:\\Database\\OHLCV.rds.gz"
}

#' loadOHLCV
#' 
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
loadOHLCV <- function(file=file.path(system.file(package = "strategery"), "data", "OHLCV.rds.gz")){
  envir <- .GlobalEnv
  out <- readRDS(file)
  assign("OHLCV",out, envir=envir)
  return(out)
}

#' saveOHLCV
#' 
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
saveOHLCV <- function(file="G:\\Database\\OHLCV.rds.gz"){
  saveRDS(OHLCV, file, compress=T)
}

#' UpdateOHLCV
#' 
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
updateOHLCV <- function(x){

  new <- as.data.table(getSymbols("SPX", src="yahoo", from="1900-01-01", auto.assign=FALSE))
  setnames(new, names(new), sub("SPX.","",names(new)))
  new[,Instrument:="SPX"]
  new[, Source:= 1L]
  setcolorder(new, c("Instrument","Date","Open", "High", "Low","Close", "Volume","Adjusted", "Source" ))
  setkey(new, Instrument, Date)
  setkey(OHLCV,NULL)
  OHLCV[, Source:= 0L]
  OHLCV <- rbindlist(list(OHLCV,new[Date>"2012-12-18"]))
  setkey(OHLCV, Instrument, Date)
  return(OHLCV)
}

#' Define universe of trading opportunities (instruments x bars)
#' 
#' Construct trading bars frame for each symbol
#' @export
Universe <- function(
  instruments #c("SPY", "AAPL")
){
  # these three lines are temporary, until organized into database
  if("SPX" %in% instruments) {
    fund("SPX", currency=currency("USD"))
    # instrument_attr("SPX",attr="OHLC",value=SPX)
    instrument_attr("SPX",attr="exchange",value="NYSE")
    loadOHLCV()
  }
  OHLCV <- OHLCV[Instrument %in% instruments]
#   u <- time.frame(instruments, bds=TRUE)
#   assign("R", copy(u), envir=.GlobalEnv)
}

Cl <- function(x){
  UseMethod("Cl")
}
Cl.xts <- function(x){
  xts:::Cl(x)
}
Cl.data.table <- Close.data.table <- function(x) {
  #filter Closes
  originalkey <- key(x)
  setkey(x,itime)
  out <- x[J(cl.time),roll=TRUE]
  setkeyv(x,originalkey)
  return(out)
}

data.source <- function(dsn, uid=NULL, pwd=NULL
                        , api=c("odbc","Rbbg"), connect=T) {
  
  ans <- structure(list(), name=dsn, uid=uid, pwd=pwd
                   , api=api, connected= FALSE, class="data.source")
  
  if(connect)
    ans <- connect(ans)
  
  return(ans)
}

connect <- function(x) {
  UseMethod("connect",x)
}

connect.data.source <- function(x) {
  
  if(attr(x, "api")=="odbc") {
    require(RODBC)
    ans <-odbcConnect(dsn=attr(x, "name"), uid=attr(x, "uid"), pwd= attr(x, "pwd"))
  }
  
  if(attr(x, "api")=="Rbbg"){
    require(Rbbg)
    ans <- blpConnect()
  }
  
  #require(xlsx)
  
  cls <- c( class(ans) , "data.source")

  structure(ans, connected=TRUE, class=cls)
  
}

#' Construct indicator class and define identifiers in the context of different data sources
#' 
#' Modeled after FinancialInstrument::
#' @param lookup list of data.source objects to look up the ids. Ordered by preference.
#' @export
financial.index <- function (primary_id, ..., currency=NA, multiplier=1
                              , tick_size = NULL, identifiers = NULL
                              , type = "financial.index", assign_i = TRUE) 
{
  if (is.null(primary_id)) {
    stop("you must specify a primary_id for the instrument")
  }
  raw_id <- primary_id
  if (substr(primary_id, 1, 1) == 1) {
    primary_id <- substr(primary_id, 2, nchar(primary_id))
  }
  primary_id <- make.names(primary_id)
  #   if (missing(currency) || is.null(currency) || (!missing(currency) && 
  #                                                    !is.currency.name(currency))) {
  #     stop("currency ", currency, " must be defined first")
  #   }
  if (!hasArg(identifiers) || is.null(identifiers)) 
    identifiers = list()
  if (!is.list(identifiers)) {
    warning("identifiers", identifiers, "do not appear to be a named list")
  }
  if (raw_id != primary_id) {
    identifiers <- c(identifiers, raw_id = raw_id)
  }
  arg <- list(...)
  if (is.list(arg[["..."]])) {
    if (length(arg) == 1) 
      arg <- arg[["..."]]
    else {
      targ <- arg[["..."]]
      arg[["..."]] <- NULL
      arg <- c(arg, targ)
    }
  }
  if (!is.null(arg$src)) {
    sarg <- list()
    sarg[[primary_id]] <- arg$src
    setSymbolLookup(sarg)
  }
  ident_str <- tolower(c("X.RIC", "RIC", "CUSIP", "SEDOL", 
                         "OSI", "Bloomberg", "Reuters", "ISIN", "CQG", "TT", "Yahoo", 
                         "Google"))
  lnarg <- tolower(names(arg))
  pos_arg <- which(lnarg %in% ident_str)
  identifiers <- c(identifiers, arg[pos_arg])
  arg[pos_arg] <- NULL
  if (!is.numeric(multiplier) || length(multiplier) > 1) {
    stop("multiplier must be a single number")
  }
  if (!is.null(tick_size) && (!is.numeric(tick_size) | length(tick_size) > 
                                1)) {
    stop("tick_size must be NULL or a single number")
  }
  if (is.null(type)) {
    tclass = "instrument"
  }
  else tclass = unique(c(type, "instrument"))
  if (is.currency.name(primary_id)) {
    oid <- primary_id
    primary_id <- tail(make.names(c(ls_instruments(), oid), 
                                  unique = TRUE), 1)
    warning(paste(oid, "is the name of a currency. Using", 
                  primary_id, "for the primary_id of this", type))
    identifiers <- c(identifiers, ticker = oid)
  }
  tmpinstr <- list(primary_id = primary_id, currency = currency, 
                   multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, 
                   type = type)
  if (length(arg) >= 1) {
    tmpinstr <- c(tmpinstr, arg)
  }
  class(tmpinstr) <- tclass
  if (assign_i) {
    assign(primary_id, tmpinstr, envir = as.environment(FinancialInstrument:::.instrument))
    return(primary_id)
  }
  else return(tmpinstr)
}


# DATA TABLE --------------------------------------------------------------

#' convert data.table to xts
#' 
#' @param x - dcast'ed data.table
as.xts.data.table <- function(x) {
  xts(as.data.frame(x[,!"Date", with=FALSE]), order.by=as.Date(as.character(x$Date)))
}
