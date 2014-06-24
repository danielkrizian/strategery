#' Backtest
#' 
#' @export
Backtest <- function() {
  
  events = Queue()
  
  market = OHLCV
  mh = MarketHandler()
  advisor = Advisor()
  port = Portfolio(instruments=INSTRUMENT$InstrumentID)
  pm = PortfolioManager(events=events, portfolio=port)
  rm = RiskManager()
  bo = BackOffice(portfolio=port, market=market)
  pa = PerformanceAnalyst(portfolio=port)
  oms = OMS()
  trader = Trader(market=market, oms=oms)
  
#   while(TRUE) {
#       mh$download()
#     while(mh$continue.backtest) {
#       
#     }
# 
#   }
  
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
