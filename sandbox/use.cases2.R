require(strategery)
require(data.table)
require(xts)

# Load data
data(SPX)
# table of instrument definitions - exchange traded
sym <- "SPX"
instrument(sym,currency=currency("USD"),multiplier=1, exchange="NYSE",assign_i=T)

newStrategy("tom")



# Trading Opportunities (universe * bars) ---------------------------------

Universe( c("SPX") )


# Indicator data ---------------------------------------------------------------
Instrument <- "SPX"
date <- as.IDate("1901-01-04")
round(Date) + 
seq(from, Date, , lengthby="day")

eom <- function(Date) {
  require(lubridate)
  
}

TurnMonth <- function(first.n.bds, last.n.bds, lag=0){

  bd <- time.frame("SPX", bds=TRUE)
  
  bd[, from.EOM:=1:length(Date)
      , by=list(Instrument, month=round(Date, "months"))]
  bd[, to.EOM:=length(Date) - from.EOM + 1
      , by=list(Instrument, month=round(Date, "months"))]
  bd[, TOM:=(from.EOM<=first.n.bds) | (to.EOM<=last.n.bds)]

  return(bd[, list(Instrument, Date, TOM)])
}

indicator("TOM", fun=TurnMonth, parameters=list(first.n.bds=3, last.n.bds=1, lag=0))

Buy <- quote(TOM==TRUE)
Sell <- quote(!Buy)

