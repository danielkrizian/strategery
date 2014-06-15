
Portfolio.position <- function(instrument=NULL, date=NULL){
  if(!length(positions))
    return(0)
  if(is.null(date))
    last(positions[Instrument==instrument,]$Pos)
  else
    last(positions[Instrument==instrument][Date<=date]$Pos)
}
#' Add transactions to the portfolio
#' 
#' @param x data.table object with columns:
#' Instrument, Date, TxnQty, Price, TxnValue
#' keyed by Instrument, Date
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
Portfolio.trades <- function(summary=T, by= NULL, incl.open=T) {

  # treat incomplete (still open) trades
  if(incl.open){
    closeout.orders = txns[, list(OrderSize=-last(Pos)),by="Instrument"][OrderSize!=0]
    # add last avaialable date
    closeout.orders = OHLCV[, list(Date=last(Date)), by=Instrument][closeout.orders]
    setkey(closeout.orders, Instrument, Date)
    closeout.txn = execute(closeout.orders, lag.days = 0)[, Pos:=0]
    .txns = rbindlist(list(txns, closeout.txn))
    setkey(.txns, Instrument, Date)
  } else {
    # remove last txn if Pos!=0
    toremove = txns[, list(Date=last(Date), Pos=last(Pos)), by="Instrument"][Pos!=0]
    .txns = txns[!setkey(toremove, Instrument, Date)]
  }
  
  .txns[, TradeID:=cumsum(delay(cumsum(TxnQty), pad=0)==0), by=Instrument]
  .trades <- .txns[, list(PL=-sum(TxnValue), 
                          Base=ifelse(first(TxnValue)>0, 
                                      sum((TxnValue>0)*TxnValue), 
                                      sum((TxnValue<0)*TxnValue)),
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

#' Keeps track of all positions with a profit and loss ("PnL").
#' keep track of the market value of the positions (known as the "holdings")
#' 
#' In addition to the positions and holdings management the portfolio must also
#' be aware of risk factors and position sizing techniques in order to optimise 
#' orders that are sent to a brokerage or other form of market access.
#' 
#' RiskManagement + OrderManagementSystem (OMS)
#' Risk Management=c("Exposure", "Position Sizing")
#' 
#' Portfolio object must be able to handle SignalEvent objects, generate 
#' OrderEvent objects and interpret FillEvent objects to update positions.
#' 
#' The SignalEvents are utilised by the Portfolio object as advice for how to
#' trade. It assesses them in the wider context of the portfolio, in terms of 
#' risk and position sizing. This leads to OrderEvents that will be sent to an 
#' ExecutionHandler.
#' 
#' Portfolio requires an initial capital value, which is set to the default of 
#' 100,000 USD. It also requires a starting date-time.
#' @field pos.history
#' Stores a list of all previous positions recorded at the timestamp of a market
#' data event. A position is simply the quantity of the asset. Negative 
#' positions mean the asset has been shorted.
#' @field pos
#' Stores a dictionary containing the current positions for the last market bar 
#' update.
#' @field holdings.history
#' Holdings describe the current market value of the positions held - 
#' closing price obtained from the current market bar, an approximation.
#' Track here also cumulative cash, cum. commissions and total equity. 
#' Short positions are treated as negative. The starting cash and total equity 
#' are both set to the initial capital.
#' @field holdings
#' Stores the most up to date dictionary of all symbol holdings values.
#' @import data.table
#' @include Event.R
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
                           updateTimeIndex = function(event){
  "Handles the new holdings tracking. It firstly obtains the latest prices from 
  the market data handler and creates a new dictionary of symbols to represent the
  current positions, by setting the 'new' positions equal to the 'current' 
  positions. These are only changed when a FillEvent is obtained, which is handled
  later on in the portfolio. The method then appends this set of current positions
  to the positions.history list. Next, the holdings are updated in a similar 
  manner, with the exception that the market value is recalculated by multiplying
  the current positions count with the closing price of the latest bar. Finally 
  the new holdings are appended to holdings.history.
  
  On every 'heartbeat', that is every time new market data is requested from the 
  DataHandler object, the portfolio must update the current market value of all 
  the positions held. In a live trading scenario this information can be 
  downloaded and parsed directly from the brokerage, but for a backtesting 
  implementation it is necessary to calculate these values manually.
  
  Arguments:
    event - MarketEvent from the events queue.
"
  # update positions
  # append to history
  # update holdings and calculate market value
  # append to history
  
                             print("Time index updated in portf from market event")
                           },
                           updateSignal = function(event){
  "Acts on a SignalEvent to generate new orders based on the portfolio logic."
  
                             print("Signal updated in portf from signal event")
                           },
                           updateFill = function(event){
  "Updates the portfolio current positions and holdings from a FillEvent.
  
  Determines whether a FillEvent is a Buy or a Sell and then updates the 
  current positions accordingly by adding/subtracting the correct quantity of 
  shares"
  
                             print("Fill updated in portf from fill event")
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