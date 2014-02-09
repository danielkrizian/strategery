
# Instrument master data - Load to R --------------------------------------
library(RODBC)
library(data.table)
library(xts)
library(TTR)
library(FinancialInstrument)
src <- data.source("FalconDB", "Falcon", "falcon", "odbc", connect=T)
rm_instruments(keep.currencies=F)
load.instruments.falcon(src,cur=T)
saveInstruments()

