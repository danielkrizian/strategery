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
prices = prices[!prices[J(Instrument="IEF",Date=as.Date("2014-02-21"))]] # simulate holiday
.data = prices[, c("Instrument","Date","Close"), with=FALSE]
setnames(.data, "Close", "Price")
# regularize frame (na.locf holidays)
frame = CJ(Instrument=unique(.data$Instrument), Date=unique(.data$Date))
leadingNA = .data[frame][,list(toremove= cumsum(!is.na(Price)) <= 0), 
                         by=Instrument]$toremove
frame = frame[!leadingNA]
.data=.data[frame, roll=Inf]

.data[, c("Txn.Qty", "Txn.Price", "Txn.Fees", "Txn.Value", "Txn.Avg.Cost",
          "Pos.Qty", "Pos.Avg.Cost", "Gross.Txn.Realized.PL", 
          "Net.Txn.Realized.PL", "Pos.Value", 
          "Period.Realized.PL","Period.Unrealized.PL","Gross.Trading.PL", 
          "Net.Trading.PL"):=0]

txnData = copy(.data)[, c("Txn.Qty", "Txn.Price", "Txn.Fees", "Price"):=list(
  TxnQty = c(5, 0, -2, 0,  -3, 0, -4, 1, 0, 3, 0, 0),
  Txn.Price = Price*0.95,
  Txn.Fees=0,
  Price=NULL
)]
txnData = txnData[!txnData[Txn.Qty==0]]
txnData[J(Instrument="DBC",Date=as.Date("2014-02-21")), Date:=Date+1] # simulate gap trade
setkey(txnData, Instrument, Date)

###### MANUAL TEST ######

# addTxns
.data[txnData, c("Txn.Qty","Txn.Price", "Txn.Fees", "Txn.Value", "Txn.Avg.Cost"):={
  Txn.Qty=i.Txn.Qty
  Txn.Price=i.Txn.Price
  Txn.Fees=i.Txn.Fees
  Txn.Value= Txn.Qty * Txn.Price + Txn.Fees
  Txn.Avg.Cost = Txn.Value / Txn.Qty
  list(Txn.Qty, Txn.Price, Txn.Fees, Txn.Value, Txn.Avg.Cost)
}, roll=-Inf]

.data[, c("Pos.Qty", "Pos.Avg.Cost", "Gross.Txn.Realized.PL", "Pos.Value",
          "Gross.Trading.PL") :={
  # TODO: don't do all dates, just subset: .data[datesubset, ...]
  # Prev.Pos.Avg.Cost = getLast("Pos.Avg.Cost",Instrument=Instrument, Date=prior_date)
  Last.Pos.Avg.Cost = 0
  Last.Pos.Qty = 0
  Last.Pos.Value = 0
  Pos.Qty = cumsum(Txn.Qty)
  Pos.Avg.Cost = calc_Pos.Avg.Cost(Last.Pos.Avg.Cost, Txn.Value, Pos.Qty)

  Gross.Txn.Realized.PL = ifelse(diff.default( abs(c(Last.Pos.Qty, Pos.Qty)) ) > 0,
                                 0, # if scaling-in => no PL
                                 Txn.Qty * (c(Last.Pos.Avg.Cost, head(Pos.Avg.Cost, - 1))
                                            - Txn.Avg.Cost))
  Pos.Value = Pos.Qty * Price
  Gross.Trading.PL = diff.default(c(Last.Pos.Value, Pos.Value)) - Txn.Value
  list(Pos.Qty, Pos.Avg.Cost, Gross.Txn.Realized.PL, Pos.Value, Gross.Trading.PL)
  }, by="Instrument"]

.data[, c("Net.Txn.Realized.PL", "Period.Realized.PL", "Period.Unrealized.PL",
          "Net.Trading.PL"):={
  Net.Txn.Realized.PL = Gross.Txn.Realized.PL + Txn.Fees
  Period.Realized.PL = Gross.Txn.Realized.PL
  Period.Unrealized.PL = Gross.Trading.PL - Period.Realized.PL
  Net.Trading.PL = Gross.Trading.PL + Txn.Fees
  list(Net.Txn.Realized.PL, Period.Realized.PL, Period.Unrealized.PL, Net.Trading.PL)
}]

summary = data.table(Date = unique(.data$Date), key = "Date")
summary[, c("Long.Value", "Short.Value", "Net.Value", "Gross.Value", 
            "Realized.PL", "Unrealized.PL", "Gross.Trading.PL", "Txn.Fees",
            "Net.Trading.PL"):=0]
initDate = "2014-02-18"
summary[Date>=initDate, c("Equity"):=1000]

minisummary = .data[, list(Long.Value=sum(max(Pos.Value, 0)),
                           Short.Value=sum(min(Pos.Value, 0)),
                           Net.Value=sum(Pos.Value),
                           Gross.Value=sum(abs(Pos.Value)),
                           
                           Realized.PL=sum(Period.Realized.PL),
                           Unrealized.PL=sum(Period.Unrealized.PL),
                           Gross.Trading.PL=sum(Gross.Trading.PL),
                           Txn.Fees=sum(Txn.Fees),
                           Net.Trading.PL=sum(Net.Trading.PL))
                           , keyby=Date]

summary[minisummary, c("Long.Value", "Short.Value", "Net.Value", "Gross.Value", 
                       "Realized.PL", "Unrealized.PL", "Gross.Trading.PL", "Txn.Fees",
                       "Net.Trading.PL"):=
          list(i.Long.Value, i.Short.Value, i.Net.Value, i.Gross.Value, i.Realized.PL, 
i.Unrealized.PL, i.Gross.Trading.PL, i.Txn.Fees, i.Net.Trading.PL)]

summary[Date>=minisummary[1]$Date,c("Equity"):=Equity + cumsum(Net.Trading.PL)]
summary


##### USING CLASS #########

p = Portfolio$new("ABC", prices=OHLCV, equity=1000, as.of=min(txnData$Date)-1)
debug(p$update)

for(d in unique(txnData[["Date"]])[order(unique(txnData[["Date"]]))]) {
  p$add_txns(txnData[Date==d])
  p$update(now=d) 
}



system.time(for (i in 1:100) {
  p$add_txns(txnData)
  p$update(now=max(txnData[["Date"]]))
}

  )
p$update(now=max(txnData[["Date"]]))
p$summary

p$.data[Net.Trading.PL !=0]

p$update()
View(p$blotter)
View(p$positions)
system.time(for(i in 1:1000) p$update("2010-02-10"))

p$state
p$summary


txnDataTest = setkey(txnData[, list(Txn.Date, Instrument, Txn.Price)],
                     Instrument, Txn.Date)

system.time(for(i in 1:1000) setkey(rbindlist(list(txnDataTest, txnDataTest), use.names = F, fill=F), 
                                    Txn.Date, Instrument))

system.time(for(i in 1:10000) is.na(1:1e+6))

AD = D[.N]
AD = data.table(symbol="aaa", country="vgvrt")
system.time(for(i in 1:1000)  out <- rbind(amzn.trades, amzn.trades))
system.time(for(i in 1:1000) P[TX, txn:=i.txn, roll=-Inf])
system.time(for(i in 1:1000) {D = rbind(D, AD);setkey(D, date, symbol)})

library(data.table)
P = data.table(id=c("a","a"), t=c(1,4), txn=c(0, 0),key=c("id", "t"))
TX = data.table(id=c("a"), t=c(3), txn=c(1111),key=c("id", "t")) # note the index t = 3 falling in the gap of P
P[TX, txn:=i.txn, roll=TRUE]
P

