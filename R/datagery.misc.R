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