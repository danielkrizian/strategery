require(strategery)
require(quantstrat)
require(data.table)
require(xts)


# have instrument data loaded
source("R\\use\\etl.instruments.R")

# select universe
symbols <- "SPX"
Universe(symbols)

cal <- time.frame(symbols, bds=TRUE) # trading days data

TurnOfMonth <- function(x, last=1, first=3) {
  eom <-endpoints(x, on="months") # end of month
  tom <- sort(as.vector(outer(eom, (-last+1):(first),"+"))) # shift backward&forward
  tom <- tom[tom > 0 & tom <= length(x)] # eliminate values outside range
  1:length(x) %in% tom
}

debugonce(`==.indicator`)
debugonce(print.indicator)

TOM <- indicator( TurnOfMonth(Date, 1, 3), data=cal)

mom <- indicator( momentum(Close, n), data=OHLCV)
cal[,TOM:=TurnOfMonth(Date, 1,3)]

nMom <- 20
momentum <- quote( mom(Close, n=nMom) > 0)

AddIndicator(momentum, "momentum")

class(calc(TOM))
TOM==TRUE
Buy <- (TOM==TRUE) %AND% (momentum > 0) # allow for 
Sell <- quote(!Buy)

Backtest()
saveStrategy()

newStrategy("fomc")
Universe( c("SPX") )

data(fomc)
# data prior 1986 unverified
fomc <- data.table(Date=as.IDate(fomc[as.Date(fomc$x) > "1986-01-01",])) 
setkey(fomc,"Date")
cal <- Calendar.Trading(exchange="NYSE")
setkey(cal,"Date")
cal[,isFOMC:=FALSE]
cal[fomc,isFOMC:=TRUE]
cal[, to.FOMC:=BarsTo(isFOMC)]

SymbolExchange <- data.table(Instrument=c("SPX"),exchange="NYSE", key="exchange")
setkey(cal,"exchange")
FOMC <- cal[SymbolExchange, allow.cartesian=TRUE
           ][isBizday==TRUE][,to.FOMC:=anticipate(to.FOMC,1)][,list(Instrument, Date, to.FOMC)]

AddIndicator(FOMC)

nMom <- 20
momentum <- quote( mom(Close, n=nMom) > 0)

AddIndicator(momentum, "momentum")
Buy <- quote( (to.FOMC<2) & momentum )
Sell <- quote(!Buy)

Backtest()
saveStrategy()