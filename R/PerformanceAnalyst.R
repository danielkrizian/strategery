#' @include Portfolio.R
#' @import lubridate
#' @import xts
PerformanceAnalyst <- setRefClass("PerformanceAnalyst",
                          fields=list(
                            portfolio="Portfolio" # ?
                          ),
                          methods=list(
                        
  returns = function(interval="days") {
    # Return = PL / abs(Prev.Value) # abs to accommodate short positions too
    
    holdings = portfolio$positions
    name = portfolio$name
    performance <- calcPL()
    performance <- performance[,list(PL=sum(PL), Prev.Value=sum(Prev.Value)), keyby=Date]
    performance[             , Return:=0]
    performance[Prev.Value!=0, Return:=PL/abs(Prev.Value)]
    performance[,Instrument:=if(length(name)) name else "Portfolio"]
    
    performance <- performance[,list(Instrument, Date, Return)]
    setkey(performance, Instrument, Date)
    r = strategery::returns(performance, col="Return")
    return(r)
  },
  
  calcPL = function(){
    
  "Calculate portfolio profit & loss for each period.
    
  Gross.Trading.PL=Pos.Value- LagValue - Txn.Value
  Period.Unrealized.PL = Gross.Trading.PL - Gross.Txn.Realized.PL"
    
    holdings = portfolio$positions
    holdings[, Prev.Value:=delay(Value, pad=0), by=Instrument]
    holdings[, PL:= Value - Prev.Value - TxnValue]
    return(holdings)
  },
  
  collectTrades = function(incl.open=T) {
    fills = portfolio$fills
    positions = portfolio$positions
    # treat incomplete (still open) positions at instant liquidation value
    if(incl.open){
       open.instr = fills[, list(Pos=last(Pos)),by="Instrument"][Pos!=0]$Instrument
      closeout.fill = 
        positions[, list(Date=last(Date), TxnQty=last(-Pos), Price=last(Price),
                         TxnValue=-last(Value), Pos=0), by=Instrument][J(Instrument=open.instr)]
      setkey(closeout.fill, Instrument, Date)

      fills = rbindlist(list(fills, closeout.fill))
      setkey(fills, Instrument, Date)
    } else {
      # remove last fill if Pos!=0
      toremove = fills[, list(Date=last(Date), Pos=last(Pos)), by="Instrument"][Pos!=0]
      fills = fills[!setkey(toremove, Instrument, Date)]
    }
    
    fills[, TradeID:=cumsum(delay(cumsum(TxnQty), pad=0)==0), by=Instrument]
    return(fills)
  },
  
  tradePL = function(){
    trades = collectTrades()
    trades <- trades[, list(PL=-sum(TxnValue), 
                            Base=ifelse(first(TxnValue)>0, 
                                        sum((TxnValue>0)*TxnValue), 
                                        sum((TxnValue<0)*TxnValue)),
                            Start=first(Date),
                            End=last(Date))
                     , by="Instrument,TradeID"]
    trades[,PL:=PL/abs(Base)]
    trades[,Side:=as.character(factor(Base>0
                                      , levels=c(T, F)
                                      , labels=c("Long","Short")))]
  },
  
  tradeStats = function(by=NULL) {
    "by character. Can be 'Instrument', 'Side', or 'Instrument,Side'"
    PLtable = tradePL()
    PLtable[, list(
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
  },
  
  summary = function(){
    print(returns()$summary())
    print(tradeStats(by=NULL))
  },
  
  plot = function(...){
    returns()$plot(...)
  },
  
  show = function(...){
    .self$summary()
    .self$plot(...)
  }
                          )
)