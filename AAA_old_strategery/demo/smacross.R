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

rule.smacross <- function(n_fast=50, n_slow=200){
  close <- Cl (market[['SPX']])
  sma_fast <- SMA(close, n=n_fast)
  sma_slow <- SMA(close, n=n_slow)

  long <-  sma_fast > sma_slow
  short <- sma_fast < sma_slow

  sig <- na.omit(lag(long - short))
  return(sig)
}

pars <- c(50,200) # you can change that if not happy with rule.smacross default values

init.strategy(s) # add default rule function
b <- Test(s, pars=pars, details=T, size=1)
b$trades
b$pos
b$R

Summary(b,stats=c("curve"),format=T)
Summary(b,stats=c("trade"),format=T)
Summary(b,stats=c("period"),format=T)