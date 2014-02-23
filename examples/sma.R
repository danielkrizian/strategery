
newStrategy("sma")

Universe(c("VTI", "VEU", "IEF", "VNQ", "DBC"))

Close <- indicator(col="Close", input=OHLCV)
SMA <- indicator( fun=SMA, input=OHLCV)

Long <- Close>SMA(Close, 200) %position% equitypct("equal")
Neutral <- Close<=SMA(Close, 200) %position% equitypct(0)
Rebalance <- EOM==TRUE %rebalance% equitypct(20) # rebalance rule: each month, rebalance to 20% equity target

Backtest()

# FEATURES: 
# Universe accepts convenience Universe("VTI", "VEU", "IEF", "VNQ", "DBC")
# data for "VTI", "VEU", "IEF", "VNQ", "DBC"
# rewrite indicator() to postpone parameters until signal
# signal based on comparison of two indicators
# equitypct
# rebalance