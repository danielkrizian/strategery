

#' Read rds files, assign to the environment.
#' @export
loadDB <- function(tables=c("INSTRUMENT","OHLCV"), path=getOption("DBpath"), envir=.GlobalEnv) {
  for(table.name in tables) {
    file=file.path(path, paste(table.name, ".rds", sep=""))
    out <- readRDS(file)
    assign(table.name,out, envir=envir)
  }
}

#' getOHLCV
#' 
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
getOHLCV.yahoo <- function(ids, from="1900-01-01"){
  require(quantmod)
  
  col.names <- c("Instrument","Date","Open", "High", "Low","Close", "Volume","Adjusted" )
  
  new <- NULL
  for(id in ids) {
    D <- as.data.table(getSymbols(id, src="yahoo", from=from, auto.assign=FALSE))
    setnames(D, names(D), sub(paste(id,".",sep=""),"", names(D)))
    D[,Instrument:=id]
    setcolorder(D, col.names)
    new <- rbindlist(list(new,D))
  }
  setkey(new, Instrument, Date)
  
  return(new)
}

getOHLCV.Bloomberg <-  function(ids, from="1900-01-01") {
  require(Rbbg)
  conn <- blpConnect()
  
  start.date <- format(as.Date(from),"%Y%m%d")
  end.date <- NULL
  fields <- c("PX_LAST")
    
  toimport <- bdh(conn, ids, fields, start.date, end.date
                  , option_names=c(  "nonTradingDayFillOption"
                                     , "nonTradingDayFillMethod")
                  , option_values=c(  "ACTIVE_DAYS_ONLY"
                                      , "NIL_VALUE"))
  toimport <- as.data.table(toimport)
  setnames(toimport, old=colnames(toimport)
           , new=c("Ticker", "Date", "PX_Last"))
  
}

getData.Bloomberg <- function(ids, fields=c("PX_LAST"), from="1900-01-01") {
  require(Rbbg)
  conn <- blpConnect()
  start.date <- format(as.Date(from),"%Y%m%d")
  end.date <- NULL
  .data <- bdh(conn, ids, fields, start.date, end.date
                  , option_names=c(  "nonTradingDayFillOption"
                                     , "nonTradingDayFillMethod")
                  , option_values=c(  "ACTIVE_DAYS_ONLY"
                                      , "NIL_VALUE"))
  .data <- as.data.table(.data)
  .data$Date <- as.IDate(.data$Date)
  newnames <- if(length(ids)==1) c("Date", "Value") else c("Ticker", "Date", "Value")
  setnames(.data, old=colnames(.data)
           , new=newnames)
  return(.data)
}

getOHLCV.rds <-  function(ids, from="1900-01-01") {
    loadDB(tables="OHLCV")
}

getInstrument.odbc <- function(ids, idfield,
                               sql="SELECT * FROM Instrument WHERE @idfield IN @ids", 
                               dsn=NULL, uid="", pwd="") {
  
  require(RODBC); require(data.table)
  if(missing(ids)) {
    sql <- sub(" WHERE @idfield IN @ids", "", sql)
  }
  else {
    sql <- sub("@idfield", "Bloomberg", sql)
    sql <- sub("@ids", paste0("('",paste0(ids, collapse="','"),"')"), sql)
  }
  channel <-odbcConnect(dsn, uid, pwd)
  as.data.table(sqlQuery(channel, sql))
}




#' this is old search instrument function dependent on FinancialInstrument package
Search <- function(pattern, category="instrument") {
  #' TODO: vectorize pattern
  View(instrument.table(find.instrument(pattern)))
}