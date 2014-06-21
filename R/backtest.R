#' Backtest
#' 
#' @export
Backtest <- function() {
  
  events = Queue()
  
  market = OHLCV
  advisor = Advisor()
  port = Portfolio()
  pm = PortfolioManager(events=events, portfolio=port)
  bo = BackOffice(portfolio=port, market=market)
  pa = PerformanceAnalyst(portfolio=port)
  oms = OMS()
  trader = Trader(market=market, oms=oms)
  
  signals = advisor$signals()
  pm$signals=signals
  orders = pm$createOrders()
  oms$orders = orders
  fills= trader$executeOrders("MOC")
  
  bo$bookFills(fills)
  bo$valueHoldings()
  port$updateHoldings()
  pa$calcPL()
  pa$collectTrades()
  return(pa)
}
