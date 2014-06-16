#' Backtest
#' 
#' @export
Backtest <- function() {

  advisor = Advisor()
  port = Portfolio()
  trader = Trader()
  
  mp = advisor$signals()
  orders = port$orders(mp)
  txns = trader$execute(orders, OHLCV, "MOC")
  port$bookTxns(txns)
  port$calcPL(market=OHLCV)

  return(port)
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
