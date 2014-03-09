

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
  # ids <- as.character(sqlQuery(ch,sql)$Bloomberg)
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

getOHLCV.rds <-  function(ids, from="1900-01-01") {
    loadDB(tables="OHLCV")
}


#' this is old search instrument function dependent on FinancialInstrument package
Search <- function(pattern, category="instrument") {
  #' TODO: vectorize pattern
  View(instrument.table(find.instrument(pattern)))
}