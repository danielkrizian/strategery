############ Portfolio #########################################################
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
#' cash = initial.capital - (pos.diff*bars['Adj Close']).sum(axis=1).cumsum()
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
                                         fills="data.table",
                                         benchmarks="data.table",
                                         positions="data.table",
                                         state="list",
                                         exposures = "data.table"
                         ), 
                         methods = list(
                           
  initialize=function(instruments, date, units, ...)  {
    if(!missing(instruments)){
      if(missing(units))
        units = 0
      if(!missing(date)){
        positions <<- data.table(Instrument=instruments, Date=date, Pos=units)
      }
      state <<- setNames(as.list(rep(0, times=length(instruments))), instruments)
    }
    initFields(...)
  },
  
  addFills = function(x){
    if(is.null(fills)) 
      fills <<- x
    else {
      fills <<- .rbind.data.table(fills, x, use.names=TRUE)
      setkey(fills, Instrument, Date)
    }
    updatePositions(x)
  },
  
  position = function(instrument=NULL, date=NULL){
    if(!length(positions))
      return(0)
    if(is.null(date))
      last(positions[Instrument==instrument,]$Pos)
    else
      last(positions[Instrument==instrument][Date<=date]$Pos)
  },
  
  updatePositions = function(x){
    x[,Pos:=position(Instrument) + cumsum(TxnQty), by=Instrument]
    if(is.null(positions))
      positions <<- x[,list(Instrument, Date, Pos)]
    else
      positions <<- .rbind.data.table(positions, x[,list(Instrument, Date, Pos)], use.names=TRUE)

    last.p = positions[, list(Pos=last(Pos)),by="Instrument"]
    state <<- setNames(as.list(last.p$Pos),last.p$Instrument)
    setkey(positions, Instrument, Date)
  },
  
  updateHoldings = function(){
    holdings = positions
    with.fills <- holdings[fills][, c(names(holdings), "TxnValue"), with=FALSE]
    no.fills <- holdings[!fills][,TxnValue:=0]
    holdings <-  .rbind.data.table(with.fills, no.fills)
    setkey(holdings, Instrument, Date)
    positions <<- holdings
    # handle missing (NA) TxnValue - is.na() alternative
    #   out <- fills[,list(Instrument,Date,TxnValue)][marked.portfolio]
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
  })
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

# portfolio.PL <- function(portfolio, txns, market=OHLCV){
#   
#   market <- market[,list(Instrument, Date, Close)]
#   setnames(market,"Close","Price")
# 
#     # marked.portfolio shows NA positions (1928-02-04)
#   marked.portfolio <- portfolio[market, roll=TRUE][, Pos.Value:=Pos * Price]
#   # handle missing TxnValue - fill zeroes alternative
#   cols <- c("Instrument", "Date", "Pos", "Price", "Pos.Value")
#   valued <- marked.portfolio[, cols, with=FALSE]
#   with.txns <- valued[txns][, c(cols, "TxnValue"), with=FALSE]
#   no.txns <- valued[!txns][,TxnValue:=0]
#   valued <-  .rbind.data.table(with.txns, no.txns)
#   setkey(valued, Instrument, Date)
#   
#   # handle missing (NA) TxnValue - is.na() alternative
#   #   out <- txns[,list(Instrument,Date,TxnValue)][marked.portfolio]
#   
#   valued[, PL:= Pos.Value - delay(Pos.Value) - TxnValue]
#   return(valued)
# }