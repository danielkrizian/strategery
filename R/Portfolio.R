
Portfolio.position <- function(instrument=NULL, date=NULL){
  if(!length(positions))
    return(0)
  if(is.null(date))
    last(positions[Instrument==instrument,]$Pos)
  else
    last(positions[Instrument==instrument][Date<=date]$Pos)
}

Portfolio.addTxns <- function(x){
  # Update portfolio positions with new transactions
  if(is.null(txns)) txns <<-x else {
    txns <<- .rbind.data.table(txns, x, use.names=TRUE)
    setkey(txns, Instrument, Date)
  }
  x[,Pos:=position(Instrument) + cumsum(TxnQty), by=Instrument]
  
  if(is.null(positions))
    positions <<- x[,list(Instrument, Date, Pos)]
  else
    positions <<- .rbind.data.table(positions, x[,list(Instrument, Date, Pos)], use.names=TRUE)
  setkey(positions, Instrument, Date)
}

Portfolio.calcPL <- function(market=OHLCV){
  
  #' Calculate portfolio profit & loss for each period
  #' 
  #' Gross.Trading.PL=Pos.Value- LagValue - Txn.Value
  #' Period.Unrealized.PL = Gross.Trading.PL - Gross.Txn.Realized.PL

  market <- market[,list(Instrument, Date, Close)]
  setnames(market,"Close","Price")
  
  # start from the first available position, not from the first market price
  START <- positions[,list(First=min(Date)), by=Instrument]
  bounded.market <- market[START][Date>=First][,First:=NULL]
  setkey(bounded.market, Instrument, Date)
  marked.portfolio <- positions[bounded.market, roll=TRUE][, Value:=Pos * Price]
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
  positions <<- valued[, PL:= Value - Prev.Value - TxnValue]
  return(positions)
}

Portfolio.plot <- function(...){
  returns()$plot(...)
}

Portfolio.returns <- function(interval="days") {
  # Return = PL / abs(Prev.Value) # abs to accommodate short positions too
  if(length(.performance))
    performance <- .performance
  else {
    if(inherits(.self, "Portfolio")) {
      if(! "PL" %in% names(positions)) positions <<- .self$calcPL()
      performance <- positions[,list(PL=sum(PL), Prev.Value=sum(Prev.Value)), keyby=Date]
      performance[             , Return:=0]
      performance[Prev.Value!=0, Return:=PL/abs(Prev.Value)]
      performance[,Instrument:=if(length(name)) name else "Portfolio"]
    }
    if(inherits(.self, "Account") & !is.null(.self$benchmarks)) {
    }
  }
  performance <- performance[,list(Instrument, Date, Return)]
  setkey(performance, Instrument, Date)
  r = strategery::returns(performance, col="Return")
  return(r)
}

Portfolio.show <- function(...){
  .self$summary()
  .self$plot(...)
}

Portfolio.summary <- function(){
  print(returns()$summary())
  print(trades(summary=T, by=NULL))
}

#' Extract trades list with optional summary stats
#' @param summary logical. Calculate summary statistics for trades
#' @param by character. Can be "Instrument", "Side", or "Instrument,Side"
#' @import lubridate
Portfolio.trades <- function(summary=T, by= NULL) {
  
  txns[, TradeID:=cumsum(delay(cumsum(TxnQty), pad=0)==0) ,by=Instrument]
  .trades <- txns[, list(PL=-sum(TxnValue), 
                        Base=ifelse(first(TxnValue)>0, sum((TxnValue>0)*TxnValue), sum((TxnValue<0)*TxnValue)),
                        Start=first(Date),
                        End=last(Date))
                 , by="Instrument,TradeID"]
  .trades[,PL:=PL/abs(Base)]
  .trades[,Side:=as.character(factor(Base>0
                                    , levels=c(T, F)
                                    , labels=c("Long","Short")))]
  
  summary.trades <- function(x, by=NULL) {
    x[, list(
      "Number of Trades"=length(PL),
      "Average Days in Trade"=mean(as.numeric(End-Start)),
      "Trades/Year"=length(PL)/(as.duration(max(End)-min(Start))/dyears(1)),
      "Average P/L"=mean(PL),
      "Average Win"=avgwin(PL, extreme=T),
      "Average Loss"=avgloss(PL),
      "Best Trade"=max(PL),
      "Worst Trade"=min(PL),
      "Win Rate"=winrate(PL),
      "Win/Loss"=winloss(PL, extreme=F),
      "Expectancy"=expectancy(PL),
      "Profit Factor"=profitfactor(PL, extreme=F)
    )
    ,by=by]
  }
  out = if(summary) summary.trades(.trades, by=by) else .trades
  
  return(out)
}

#' @import data.table
Portfolio <- setRefClass("Portfolio"
                         , fields = list(name="character",
                                         positions="data.table",
                                         txns="data.table",
                                         exposures = "data.table" 
                         )
                         , methods = list(
                           
                           initialize=function(...)  {
                             assign('.performance',numeric(), .self)
                             .self$initFields(...)
                           },
                           position = Portfolio.position, 
                           addTxns = Portfolio.addTxns,
                           calcPL = Portfolio.calcPL,
                           plot = Portfolio.plot,
                           returns = Portfolio.returns,
                           show = Portfolio.show,
                           summary = Portfolio.summary,
                           trades = Portfolio.trades)
)


#' constructor for creating an portfolio object.
#' 
#' Portfolio object represents implementation of a strategy, and hence have the same name.
portfolio <- function(data) {
  
  if(missing(data))
    return(Portfolio$new())
  
  Portfolio$new(positions=data)
}

# update.portfolio <- function(portfolio, txns) {
#   .LastPos <- function(portfolio, Instrument) {
#     LastPos <- last(portfolio[Instrument==Instrument,])$Pos
#     return(LastPos)
#   }
#   txns[,Pos:=.LastPos(portfolio, Instrument) + cumsum(TxnQty), by=Instrument]
#   portfolio <- .rbind.data.table(portfolio
#                                  , txns[,list(Instrument, Date, Pos)], use.names=TRUE)
#   setkey(portfolio, Instrument, Date)
#   
#   return(portfolio)
# }

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