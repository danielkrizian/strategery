

#' Backtest
#' 
#' @export
Backtest <- function() {
  
  extract.signal <- function(rule) {
    s = rule$signal
    if(is.sfl(s))
      s = eval.sfl(s)
    s$data[eval(as.name(s$.col)) == TRUE, c(s$.id, s$.time), with=FALSE]
  }
  
  model.portfolio <- function(signal, sizing) {
    if(is.numeric(sizing))
    signal[, Pos:=sizing]
  }
  
  # evaluate all rules into model portfolios and 
  # reconcile them into single model portfolio
  mp = NULL
  for(r in ls_rules()) {
    r = get(r)
    s = extract.signal(r)
    p = model.portfolio(s, r$size)
    mp <- if (is.null(mp)) p
    else
      .rbind.data.table(mp, p, use.names=TRUE)
  }
  
  setkeyv(mp, names(mp)[-length(names(mp))])
  
  if(nrow(mp[duplicated(mp)])) {
    print("Ambiguous position signals found at: ")
    print(mp[duplicated(mp)])
    browser()
  }
  
  # apply rules in an existing portfolio, generate orders from signals 
  # signal = difference between existing and model portfolio
  generate.orders <- function(model, portfolio=NULL) {
    orders = model[, OrderSize:=c(Pos[1],diff.default(Pos)), by=Instrument]
    orders[OrderSize!=0][,Pos:=NULL]
  }
  
  orders = generate.orders(mp)
  txns <- execute(orders, OHLCV, "MOC")
  
  if(nrow(txns[duplicated(txns)])){
    print("Multiple transactions generated for the same instrument: ")
    print(txns[duplicated(txns)])
    print("Backtest stopped to prevent error in Portfolio$calcPL. TODO: make it robust.")
    browser()
  }
     
  # initDate <- min(orders$Date)
  # portfolio <- data.table(Date=initDate, Instrument=unique(orders$Instrument), Pos=numeric(1), key="Date")
  
  # update portfolio positions with new transactions
  # otherwise initial portfolio could be passed in via dots
  btportfolio <- new("Portfolio")
  btportfolio$addTxns(txns)
  btportfolio$calcPL(market=OHLCV) #market=ohlc

  a <- new("Account",portfolios=list(btportfolio))
  
  return(a)
}

#' Some Title
#' 
#' @export
Summary <- function
(portfolio, # portfolio object, having components R, pos and trades
 format=F,
 ... # other arguments passed to format.stats function
){
  c("curve", "trade", "period")
  stats <- switch(stats, 
                  curve=
                    c('Total Return','CAGR','Sharpe','Sortino','Volatility','DVR','MAR','Max Daily Drawdown','Average Drawdown','Avg Drawdown Length','Avg Trades Per Year'),
                  trade=
                    c('Trade Winning %','Average Trade','Average Win','Average Loss','W/L Ratio','Best Trade','Worst Trade','Avg Days in Trade','Expectancy','Profit Factor'),
                  period=
                    c('Time In Market','% Winning Months','Average Winning Month','Average Losing Month','Best Month','Worst Month','% Winning Years','Best Year','Worst Year','Positive 12 Month Periods'),
                  stats)
  
  if(is.null(portfolio$R))
    portfolio$R <- Returns(portfolio)
  if(is.null(portfolio$trades))
    portfolio$trades <- Trades(portfolio)
  
  
  
  return(out)
}
