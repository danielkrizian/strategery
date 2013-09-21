# idea: user wants to inspect/sanity check how indicators were calculated inside rule function and how they aggregate up to signal vector
# possible method: inspect.signals function will call Signals function

inspect.signals <- function(strategy) {
  
  #deparse rule function and extract indicators
  # call Signals
  # output xts:
#            signal     indicator1        indicator2
#timestamp     1           100               890  
 }

#### USE CASE
library('strategery')
Sys.setenv(TZ = "GMT")

s <- "smacross"
symbols <- c('SPX')
data(SPX)
market <- Prepare.market(symbols,
                         lookup=.GlobalEnv,
                         align=c('remove.na'),
                         fill.gaps=F)

strategy(s, assets=symbols, store=T)

# rule.smacross <- function(n_fast=50, n_slow=200){
#   close <- Cl (market[['SPX']])
#   sma_fast <- lag(SMA(close, n=n_fast), na.pad=F)
#   sma_slow <- lag(SMA(close, n=n_slow), na.pad=F)
# 
#   long <-  sma_fast > sma_slow
#   short <- sma_fast < sma_slow
# 
#   sig <- long - short
#   return(sig)
# }
n_fast=50
n_slow=200  
close <- Cl (market[['SPX']])
  sma_fast <- SMA(close, n=n_fast)
  sma_slow <- SMA(close, n=n_slow) 
long <-  sma_fast > sma_slow
short <- sma_fast < sma_slow
sig <- long - short

tail(merge(close,sma_fast,sma_slow,long,short,sig),600)
Signals