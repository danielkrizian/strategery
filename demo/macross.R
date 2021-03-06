#' Moving average crossover strategy with ATR position sizing
#' Buy when faster-moving average (10) is above the slower-moving average (100)
#' Trend filter: two moving average lines slower 100-days and faster 50-days.
#' Exit position otherwise.
#'
#' Clenow, Andreas F. (2012-11-26).
#' Following the Trend: Diversified Managed Futures Trading (Wiley Trading) 
#' (Kindle Locations 1515-1516). Wiley. Kindle Edition. 

library(strategery); library(TTR); library(data.table)

Universe("VTI", "IEF", "VNQ", "DBC")

fastn = 10
slown = 100
filter.fastn = 50
filter.slown = 100

OHLCV[, c("fastMA", "slowMA", "filter.fastMA", "filter.slowMA", "ATR", "P", "Long", "Neutral","weight"):={
  
  fastMA = SMA(Close, fastn)
  slowMA = SMA(Close, slown)
  filter.fastMA = SMA(Close, filter.fastn)
  filter.slowMA = SMA(Close, filter.slown)
  ATR = atr(High, Low, Close, n=14, ma="EMA")
  P = Close
  trend = filter.fastMA > filter.slowMA
  Long = (fastMA %crossover% slowMA) & trend
  Neutral = (fastMA<=slowMA) | !trend
  
  weight=0.002/(ATR/P) # size each position to 20bps daily risk
  
  out=list(fastMA, slowMA, filter.fastMA, filter.slowMA, ATR, P, Long, Neutral, weight)}, by="Instrument"]

bt = run()
bt
View(to.quarterly(bt$history$summary, OHLC=F))
View(bt$txns)


Visualize(ids="IEF")

Backtest()$tradePL()

$trades(summary=F, incl.open=F)
