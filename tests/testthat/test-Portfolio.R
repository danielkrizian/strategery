library(testthat)
library(R6)
source('R/Portfolio.R')
source('F:\\Google Drive\\RProjects\\rba\\R\\utils.R')
source('F:\\Google Drive\\RProjects\\rba\\R\\TimeSeries.R')
source('F:\\Google Drive\\RProjects\\rba\\R\\Prices.R')
require(strategery); require(xts); require(blotter)
Universe("VTI", "IEF", "VNQ", "DBC") 

##### DATA ######

prices=setkey(OHLCV[Date > c("2014-02-18") & Instrument %in% c("DBC","IEF"), nomatch=0], Instrument, Date)
prices = dcast.data.table(prices, Date ~Instrument, value.var = "Close")
prices = xts(prices[, -1, with=F], order.by=as.Date(as.character(prices$Date)))


txns = list(list(Date = as.Date("2014-02-19"), 
                 Instrument = "DBC", 
                 Qty = 5, 
                 Price = 24.909), 
            list(Date = as.Date("2014-02-19"),
                 Instrument = "IEF",
                 Qty = -4, 
                 Price = 96.786),
            list(Date=as.Date("2014-02-20"),
                 Instrument = "IEF",
                 Qty = 1, 
                 Price = 96.6815),
            list(Date = as.Date("2014-02-22"),
                 Instrument = "DBC", 
                 Qty = -2, 
                 Price = 24.89),
            list(Date = as.Date("2014-02-24"),
                 Instrument = "IEF", 
                 Qty = 3, 
                 Price = 96.7100),
            list(Date=as.Date("2014-02-25"),
                 Instrument = "DBC", 
                 Qty = -3, 
                 Price = 24.8805))

txns = Reduce(function(x,y) rbind(x,y), txns, init = data.table(Date=as.Date(character(0)),
                                                          Instrument=character(0),
                                                          Qty=numeric(0),
                                                          Price=numeric(0)))
setkey(txns, Date, Instrument)



##### USING CLASS #########

p = Portfolio$new("ABC", prices=prices, equity=100000)

  # p$add_txns(txns[n]$Instrument, txns[n]$Date, txns[n]$Qty, txns[n]$Price)
  vapply(1:NROW(prices), function(bar) {
    txns
    new_price = prices[bar]
    now = index(new_price)
    txns_now = txns[Date > p$t & Date <= now]
    if(length(txns_now))
      Map(function(i, t, q, pr) {p$add_txns(i, t, q, pr)},
          txns_now$Instrument, txns_now$Date, txns_now$Qty, txns_now$Price)
    p$update(now, new_price)
    numeric(0)
  }, FUN.VALUE=numeric(0))
debug(p$sizing)
p$update(as.Date("2014-02-19"))
p$sizing(c(-T,F), c("DBC","IEF"))

View(p$history$summary)
View(p$txns)



