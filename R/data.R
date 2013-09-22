#' blabla
#' 
#' Exchange Open and close times
#' This should be ticker-specific
#' TODO: Currently Package global variable. Rework to be ticker/exchange specific.
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
showInstruments <- function(){
  require(data.table)
  Market<- as.data.table.OHLC(SPX)
  R <- Market
  R[,Raw:=ROC(Close), by=Instrument]
  R <<- R
  return(as.character(R[,list(Unique=first(Instrument)),by=Instrument][,list(Instrument)]))
}

#' blabla
#' 
#' Exchange Open and close times
#' This should be ticker-specific
#' TODO: Currently Package global variable. Rework to be ticker/exchange specific.
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
as.data.table.xts <- as.data.table.OHLC <- function(x){
  
  require(quantmod)
  if(is.OHLC(x)) {
    op <- as.numeric(x[,has.Op(x,which=T)])
    cl <- as.numeric(x[,has.Cl(x,which=T)])
  }
  
  #http://r.789695.n4.nabble.com/data-table-and-time-series-subsetting-td4633223.html
  #http://stackoverflow.com/questions/17345951/data-table-time-subset-vs-xts-time-subset
  name <- ifelse(is.null(attr(x,"name")),as.character(substitute(x)))
  
  DT <- data.table(Instrument=name, idate=as.IDate(index(x)), Open=op, Close=cl)
  
  setkey(DT,"Instrument","idate")
  
  return(DT)
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