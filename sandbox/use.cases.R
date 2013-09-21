require(strategery)
require(data.table)
require(xts)
data(SPX)


#options
options(key=c("Instrument","idate"))
options(BuyPrice=data.table(Instrument="SPX"
                            , idate=as.IDate(index(SPX))
                            , BuyPrice=as.vector(SPX$SPX.Close)
                            , key=c("Instrument","idate")))
options(BuyPrice=quote(Close), SellPrice=quote(Open), CoverPrice=quote(Open), ShortPrice=quote(Open))
options(TradeDelays=list(Buy=1,Sell=1,Short=1,Cover=1))


#pre-processing

showInstruments()
#options(instruments=c("SPY", "AAPL"))

#Buy & Hold
Buy <- quote(TRUE)
Sell <- quote(FALSE)

#TAA
Buy  <- data.table(Instrument="SPX",idate=as.IDate("1928-01-04"), Buy=1, key=getOption("key"))
Sell <- quote(FALSE)

#Simple MA crossover
n<-2
Buy <- quote(Cross(Close, SMA(Close,n)))
Sell <- quote(Cross(SMA(Close,n), Close))


AddColumn(quote(SMA(Close,n)), "SMA 2")

View(Backtest())


