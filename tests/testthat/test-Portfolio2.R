library(testthat)
library(R6)
source('R/Portfolio.R')
source('F:\\Google Drive\\RProjects\\rba\\R\\utils.R')
source('F:\\Google Drive\\RProjects\\rba\\R\\TimeSeries.R')
source('F:\\Google Drive\\RProjects\\rba\\R\\Prices.R')
require(strategery); require(xts)
Universe("VTI", "IEF", "VNQ", "DBC") 
D = copy(OHLCV)
D = D[Date > "2014-02-20" & Instrument %in% c("VTI", "IEF")]
D[, Price:=Close][, c("Open", "High", "Low", "Close", "Volume", "Adjusted"):=NULL]
D[, Txn.Qty:= sample(c(rep(0, times=40), -10:10), size = .N)]
D

system.time(OHLCV[, list(a=sum(Close)), keyby=Date])


