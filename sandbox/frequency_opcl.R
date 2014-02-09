#' Exchange Open and close times
#' This should be ticker-specific
#' TODO: Currently Package global variable. Rework to be ticker/exchange specific.
op.time <- 090000L
cl.time <- 170000L

#' Convert xts to data.table
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
  if(FALSE) {
    opDT <- data.table(Instrument=name, idate=as.IDate(index(x)),itime=op.time, Price=op)
    clDT <- data.table(Instrument=name, idate=as.IDate(index(x)),itime=cl.time, Price=cl)
    DT <- rbindlist(list(opDT,clDT))
    setkey(DT,"Instrument","idate","itime")
  }
  
  DT <- data.table(Instrument=name, idate=as.IDate(index(x)), Open=op, Close=cl)
  
  setkey(DT,"Instrument","idate")
  
  return(DT)
}