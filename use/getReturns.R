sourceDir(".")
library(RODBC)
library(data.table)
library(xts)
library(TTR)
library(FinancialInstrument)
#library(quantstrat)
options(eq.base=100)

loadInstruments(dir="W:/Applications/Development/R/hfquant/data/")
instrument.table(find.instrument("Sirios"))

src <- data.source("FalconDB", "Falcon", "falcon", "odbc", connect=T)

p <- portfolio(find.instrument("Equity Dynamic Beta"), interval="weeks")

regions <- c("NDDLNA Index", "NDDLE15 Index", "NDDLJN Index", "NDLEEGF Index")

names(regions) <- sapply(regions, function(x) getInstrument(x)$short.name)
 
r <- returns( regions, interval="weeks")

req
cast.r <- dcast.data.table(r, Date ~ Name, drop=FALSE, fill=as.double(NA))


gdp.us <- prices(src, instruments="GDP.CQOQ.index")
ViewSubset(a,instruments="Troj")

# Instrument By Year Table ------------------------------------------------


data[,c("Year"):=year(Date)]
data.melt <- melt(data
                  , id=c("Instrument","Year")
                  , measure="Return"
                  , na.rm=TRUE)

t.InstrumentXYear <- dcast.data.table(data.melt
                                       , Instrument ~ Year
                                       , fun.aggregate=period.return
                                       , drop=FALSE
                                       , fill=as.double(NA))
t.InstrumentbXYear <- format.table(t.InstrumentXYear, percent=T)


# Fund Performance And Risk Statistics ------------------------------------

performance <- monthly[,
     list(
      'Period'=period(min(Date), max(Date))
      ,'Months'=do.call('nmonths',list(x=Date))
      ,'Total Return'=as.percent(do.call('total.return',list(R=Return)),decimals=2)
      ,'CAGR'=as.percent(do.call('cagr',list(R=Return, allow.incomplete=F, ann=12)),decimals=2)
      ,'StDev'=as.percent(do.call('stdev', list(R=Return, ann=12)),decimals=2)
      ,'Sharpe'=round(do.call('sharpe', list(R=Return, ann=12)),2)
      ,'Trend (6m)'=as.percent(do.call('mom', list(R=Return, smooth.n=6)),decimals=2)
      ,'Drawdown'=as.percent(last(do.call('drawdown', list(R=Return))),decimals=2)
       ),
     by=Instrument]

# TODO: Ordering into organize.table
View(performance[order(as.numeric(performance[["Trend (6m)"]]))])

# Timeframe to weekly ------------------------------------

odbcClose(ch)
odbcGetErrMsg(ch)
odbcTables(ch)

