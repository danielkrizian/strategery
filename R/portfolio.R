
#' @include assets.R
Portfolio <- setRefClass("Portfolio"
                         , contains="Assets"
                         , fields = list(txns="data.table"
                                         , exposures = "data.table" 
                         )
                         , methods = list(
                           
                           initialize=function(...)  {
                             assign('.performance',numeric(), .self)
                             .self$initFields(...)
                           },
                           
                           position = function(instrument=NULL, date=NULL){
                             if(!length(assets))
                               return(0)
                             last(assets[Instrument==instrument,]$Pos)
                           }, 
                           
                           addTxns = function(x){
                             # Update portfolio positions with new transactions
                             if(is.null(txns)) txns <<-x else {
                               txns <<- .rbind.data.table(txns, x, use.names=TRUE)
                               setkey(txns, Instrument, Date)
                             }
                             x[,Pos:=position(Instrument) + cumsum(TxnQty), by=Instrument]
                             
                             if(is.null(assets))
                               assets <<- x[,list(Instrument, Date, Pos)]
                             else
                               assets <<- .rbind.data.table(assets, x[,list(Instrument, Date, Pos)], use.names=TRUE)
                             setkey(assets, Instrument, Date)
                           },
                           
                           calcPL = function(market=OHLCV){
                             
                             #' Calculate portfolio profit & loss for each period
                             #' 
                             #' Gross.Trading.PL=Pos.Value- LagValue - Txn.Value
                             #' Period.Unrealized.PL = Gross.Trading.PL - Gross.Txn.Realized.PL
                             
                             
                             market <- market[,list(Instrument, Date, Close)]
                             setnames(market,"Close","Price")
                             start <- min(assets[,.SD[1] ,by=Instrument]$Date) # start from the first available position, not from the first market price
                             marked.portfolio <- assets[market[Date>=start], roll=TRUE][, Value:=Pos * Price]
                             # handle missing TxnValue - fill zeroes alternative
                             cols <- c("Instrument", "Date", "Pos", "Price", "Value")
                             valued <- marked.portfolio[, cols, with=FALSE]
                             with.txns <- valued[txns][, c(cols, "TxnValue"), with=FALSE]
                             no.txns <- valued[!txns][,TxnValue:=0]
                             valued <-  .rbind.data.table(with.txns, no.txns)
                             setkey(valued, Instrument, Date)
                             # handle missing (NA) TxnValue - is.na() alternative
                             #   out <- txns[,list(Instrument,Date,TxnValue)][marked.portfolio]
                             valued[, Prev.Value:=delay(Value, pad=0), by=Instrument]
                             assets <<- valued[, PL:= Value - Prev.Value - TxnValue]
                             return(assets)
                           })
)


#' constructor for creating an portfolio object.
#' portfolio object represents implementation of a strategy, and hence have the same name.
portfolio <- function(data) {
  
  if(missing(data))
    return(Portfolio$new())
  
  Portfolio$new(assets=data)
}



update.portfolio <- function(portfolio, txns) {
  .LastPos <- function(portfolio, Instrument) {
    LastPos <- last(portfolio[Instrument==Instrument,])$Pos
    return(LastPos)
  }
  txns[,Pos:=.LastPos(portfolio, Instrument) + cumsum(TxnQty), by=Instrument]
  portfolio <- .rbind.data.table(portfolio
                                 , txns[,list(Instrument, Date, Pos)], use.names=TRUE)
  setkey(portfolio, Instrument, Date)
  
  return(portfolio)
}

# Portfolio <- setRefClass("Portfolio", 
#                          fields = list(holdings = "numeric", 
#                                        value=function(v) {
#                                          sum(holdings)
#                                        })
# )
# 
# Portfolio$new(holdings =c(1055.43, 345.7))
# p$value
# p$value
# Portfolio
# 
# pos <- setRefClass("pos", 
#                          fields = list(last = "numeric")
# )
# portf <- setRefClass("portf", 
#                          fields = list(pos = "pos")
#                      , methods = list(lastpos = function(v=0) {
#                        pos$last
#                      }))
# 
# opos <- pos$new(last=1)
# oportf <- portf$new(pos=opos)
# oportf$lastpos()

portfolio.PL <- function(portfolio, txns, market=OHLCV){
  
  market <- market[,list(Instrument, Date, Close)]
  setnames(market,"Close","Price")

    # marked.portfolio shows NA positions (1928-02-04)
  marked.portfolio <- portfolio[market, roll=TRUE][, Pos.Value:=Pos * Price]
  # handle missing TxnValue - fill zeroes alternative
  cols <- c("Instrument", "Date", "Pos", "Price", "Pos.Value")
  valued <- marked.portfolio[, cols, with=FALSE]
  with.txns <- valued[txns][, c(cols, "TxnValue"), with=FALSE]
  no.txns <- valued[!txns][,TxnValue:=0]
  valued <-  .rbind.data.table(with.txns, no.txns)
  setkey(valued, Instrument, Date)
  
  # handle missing (NA) TxnValue - is.na() alternative
  #   out <- txns[,list(Instrument,Date,TxnValue)][marked.portfolio]
  
  valued[, PL:= Pos.Value - delay(Pos.Value) - TxnValue]
  return(valued)
}


mtm <- function(portfolio, txns, market=OHLCV) {
  txns[market]
} 

enter <- function (){
  
}

buy <- function(){
  
}