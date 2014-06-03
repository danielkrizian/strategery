# FEATURES: 
# signal based on comparison of two indicators
# equitypct position sizing
# rebalance
require(strategery);   require(rChartsDygraphs)
options(lazy.indicators=T)
options(param.indicators=T)


newStrategy("sma")

Universe("VTI", "VEU", "IEF", "VNQ", "DBC") # load.path="G:/Database"

Close = indicator(Close, data=OHLCV)

nsma = 200
SMA = indicator( SMA(Close, nsma), data=OHLCV)

Long = (Close>SMA) %position% shares(1) # equitypct("equal")
Neutral = (Close<=SMA) %position% shares(0) # equitypct(0)

# Rebalance <- (EOM==TRUE) %rebalance% equitypct(20) # rebalance rule: each month, rebalance to 20% equity target

Visualize(ids="IEF")

Backtest()




