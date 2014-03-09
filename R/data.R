# INSTRUMENT --------------------------------------------------------------
#' Fetch data about instruments from local file database (INSTRUMENT.rds file)
#'
#' Searches in any name field, not only InstrumentID.
#' @param pattern character string to search
#' @param exact logical. Whole pattern must match exactly?
#' @param best logical. If multiple records match, take top one?
#' @export
get.instrument <- function(pattern, field, exact=F, best=T) {
  INSTRUMENT[InstrumentID %like% pattern | Name %like% pattern | 
               LongName %like% pattern][1][[field]]
}

#' @export
get.holidays <- function(exchange, years=1800:2015) {
  if(identical(exchange,"NYSE")) {
    zone <- "NewYork"
    FinCenter <- "NewYork"
    holidayFUN <- "holidayNYSE"
  }
  holidays <- as.Date(do.call(holidayFUN, list(year=years)))
}

# CALENDAR ----------------------------------------------------------------

#' Construct an Exchange-specific calendar of trading (business) days
#' 
#' Table of exchange definitions -  contains trading bars
#' Warning: Saturdays before 1952-09-29 not yet accomodated
#' @source NYSE Holidays: http://www.nyse.com/pdfs/closings.pdf
#' @source NYSE Trading Hours (Saturdays before 1952-09-29): http://www.nyse.com/pdfs/historical_trading_hours.pdf
#' @export
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

  holidays <- get.holidays(exchange, years=years)

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
    IE <- INSTRUMENT[InstrumentID==symbols][,list(InstrumentID,Exchange)]
    setnames(IE, "InstrumentID", "Instrument")
    #     setkey(IE,Exchange)
    #     ED <- rbindlist(lapply(unique(IE)$Exchange,
    #                            function(e) data.table(Exchange=e
    #                                                   , Date=calendar(e))))
    #Exchange, Date data.table
    #     setkey(ED, Exchange)
    
    #     ID <- IE[ED][,list(Instrument, Date)] # Instrument, Date data.table
    ID <- IE[,data.table(Date=calendar(Exchange)), by=Instrument]
  } else {
    ID <- CJ(Instrument=symbols, Date=calendar())
  }
  setkey(ID, Instrument, Date)
  return(ID)
}



# INSTRUMENTS -------------------------------------------------------------------

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

#' Define universe of trading opportunities (instruments by bars)
#' 
#' Loads INSTRUMENT and OHLCV data for trading. Subsets by instruments specified. 
#' Constructs trading bars frame for each instrument
#' @export
Universe <- function(..., load.path=file.path(system.file(package = "strategery"), "data")){

  instruments <- as.character(unlist(list(...)))

  if(missing(load.path)) {
    warning("Database path not provided, hence using 'strategery' package default database. 
            You can set the path via options(DBpath='your path') or provide it via 
            load.path argument." , immediate. = TRUE)
#     options(DBpath=file.path(system.file(package = "strategery"), "data"))
  }
     
  # is.null(getOption("DBpath")) & 
  # load data
  loadDB(tables=c("INSTRUMENT","OHLCV"), path=load.path)
  
  # subset universe
  OHLCV <<- OHLCV[Instrument %in% instruments]

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
