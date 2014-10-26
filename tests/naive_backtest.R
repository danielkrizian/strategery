require(strategery)
options(lazy.indicators=T)
options(param.indicators=T)

Universe("SPX") # load.path="G:/Database"
OHLCV = tail(OHLCV,4)

Close = indicator(Close, data=OHLCV)

Long = (Close>0) %position% shares(1) # equitypct("equal")
Neutral = (Close<0) %position% shares(0) # equitypct(0)


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
pa

Backtest()



Advisor()$signals()