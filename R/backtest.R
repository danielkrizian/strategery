#' Backtest
#' 
#' @export
Backtest <- function() {
  
  events = Queue()
  market = OHLCV
  port = Portfolio(instruments=INSTRUMENT$InstrumentID)

  mh = MarketHandler(events=events)
  advisor = Advisor(events=events)
  pm = PortfolioManager(events=events, portfolio=port)
  rm = RiskManager()
  bo = BackOffice(portfolio=port, market=market)
  pa = PerformanceAnalyst(portfolio=port)
  oms = OMS()
  trader = Trader(market=market, oms=oms)
  
  # continue backtest until portfolio date reaches last market bar
  # once reached, keep checking for new market bars
  # backtesting
  # feeding real time
  # trading
  while(TRUE) {
      mh$updateBars() # keep checking for new bars, append
    while(mh$continue.backtest) { # while portfolio
      switch(attr(events$peek(),"event.type"),
             "market"=advisor$signals(),
             "signal"=pm$createOrders(),
             "order"=trader$executeOrders("MOC"),
             "fill"={
               bo$bookFills()
               bo$valueHoldings()
               port$updateHoldings()

               mh$backtest=FALSE
             }
      )
    }
  }
  
  pa$calcPL() # evaluate whole backtest once it's done (exited while loop)
  pa$collectTrades()
  return(pa)
}
