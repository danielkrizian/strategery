#'Generate signals
#'
Signals <- function
(
  strategy,
  pars,
  granular=F, # increase granularity of signals, costly operation
  sim.gaps=T
){
  # TODO: weekend signals - they currently get erased with Align to market prices
  # returns no NAs, range within indicator availability

    if(is.character(strategy))
      strategy <- getStrategy(strategy)
    sigFUN <- strategy$signals[['signal']]$name
    sig <- do.call(sigFUN, as.list(pars))
    
  if(granular) {
    # increase granularity of positions, if pricing more frequent
    # case:
    # sig price  ->  sig price -> pos price
    #  1    P         1    P       0    P
    #       P         1    P       1    P #we have more data for pos*ret
    #  0    P         0    P       1    P
    sig <- Align(sig, to=market$prices,pad=na.locf)
  }
  
  if(sim.gaps & market$has.NAs) {
    # simulate effect of illiquid market (NAs)
    # delay execution, as it is impossible to execute on market gap 
    # case:
    # sig price  ->  sig price -> pos price
    #  1    P         1    P       0    P
    #  0   NA         1   NA       1   NA   #unsuccessful fill of signal
    #  0    P         0    P       1    P   #correctly, still in market
    #  0    P         0    P       0    P
    sig <- sig * market$prices / market$prices # sig now contains NAs
    sig <- na.locf(sig,na.rm=F) #postpone execution due to illiquid market
  }    
  return(sig)
}

#' Some Title
#' 
#' 
Execute <- function
(
  sig,
  size=1,
  portfolio=NULL,
  dates=NULL,
  do.lag=T,
  details=F,
  ...
) {
  sig <- sig*size
  if (do.lag) {
    # simulate execution fo signals, resulting in positions 
    # case:
    # sig price  -> pos price
    #  1    P        0    P
    #  0    P        1    P   
    #  0    P        0    P 
    pos <- lag(sig, do.lag, na.pad=T)
    pos[1:do.lag,] <- 0
  } else
    pos <- sig
  
  if(length(dates))
    pos <- pos[dates]
  
  if(is.null(portfolio))
    portfolio <- portfolio(store=F)
  portfolio <- updatePfolio(portfolio,positions=pos, trades=details, store=F)
  
  return(portfolio)
}
#' Some Title
#' 
Test <- function
(
  strategy,
  pars,
  dates=NULL,
  size=1,
  portfolio=NULL,
  details=F,
  returns=T
){
  if(is.character(strategy))
    strategy <- getStrategy(strategy)
  
  sig <- Signals(strategy,pars=pars)
  
  ### TODO: sizing
#   size <- strategy$rules$someSizingFUN(someSizingPars)
  if(is.vector(size) & length(size)==1)
    size <- size
  else stop("Size argument in unrecognized format. TODO sizing.")
  oPfolio <- Execute(sig=sig,
                       size, portfolio=portfolio, dates=dates, details=details)
  if(returns)
    oPfolio$R <- Returns(oPfolio, type="periods",reduce=F, refresh=T)
  return(oPfolio)
}
#' Some Title
#' 
Train <- function
(
  strategy,
  dates=NULL,
  fitFUN=compute.edge,
  control=list(strategy=2,VTR=-4,NP= 30,itermax =10,CR=0.7,F=0.8,parallelType=2)
){
  require(DEoptim)
  require(iterators)
    
  if(is.character(strategy))
    strategy <- getStrategy(strategy)
  
  f <- function(pars, strategy, dates=NULL) {
    ans <- compute.edge(strategy, pars, dates=dates)
#     message(ans,"pars:",paste(pars,collapse="-"))
#     if(is.na(ans) | !length(ans) | is.nan(ans)) ans <- Inf
    return(ans)
  }
  ###TODO: mapping from real to distribution
  #   ret[3] <- roundstep(ret[3],5) ; if(ret[3]==0) ret[3]=1
  outDEoptim <- DEoptim(f,
                        getParBounds(strategy)$lower,
                        getParBounds(strategy)$upper, control, 
                        strategy=strategy, dates=dates,
                        fnMap=function(pars) round(pars))
  ret <- list()
  ret$pars <- outDEoptim$optim$bestmem
  ret$opt <- outDEoptim$optim
  return(ret)
}



# calculate efficiency ratio = cagr.oos / cagr.iis
# compare oos vs. following iis - iis should be better due to fitting
# compare oos vs. previous iis - sometimes oos is better due to more profit opportunities
# see Howard B. Bandy (2012): Developing Robust Trading Systems, with Implications for Position Sizing and System Health

# 1. periods iis/oos
# 2. find best pars iis
# 3. apply pars oos
# 4. next

#' Some Title
#' 
WalkForward <- function
(
  strategy,
  train.years=5, # look-back period to train parameters
  test.years=1,
  fitFUN=compute.edge,
  dates=NULL,
  ... # arguments passed to backtest function
){
  #TODO: out list of train periods, test periods
  #TODO: out list of signals (needed for trades statistics)
  require(lubridate)
  if(is.character(strategy))
    strategy <- getStrategy(strategy)
    
  data <- if(length(dates)) market$prices[dates] else market$prices
  test.periods <- split(data, f="years", k=test.years)
  test.periods[1:(train.years+1)] <- NULL # 
  folds <- unlist(lapply(test.periods, function(x) paste(range(index(x)),collapse="::")))
  
  P <- portfolio( paste("wf",strategy$name,sep="-"), store=F)
  
  for(fold in folds) {
    test.start<- as.Date(unlist(strsplit(fold,split="::"))[1])
    test.end <- as.Date(unlist(strsplit(fold,split="::"))[2])
    
    train <- paste(test.start-years(train.years), test.start-days(1), sep="::")
    train <- Train(strategy, fitFUN=fitFUN, dates=train, ...=...)
    
    message("Testing on fold: ",fold)
    P <- Test(strategy, train$pars, dates=fold, portfolio=P, details=F)
    
    B <- Benchmark(type="Random", portfolio=P)
    R <- Returns(P,B)[fold]
    stat.test <- compute.premium(R[,1],R[,2])
    message("Performance: ", stat.test)
    
    fit.pars <- xts( t(train$pars), 
                     order.by=test.end)
    stat.train <- xts(train$opt$bestval,
                       order.by=test.end)
    stat.test <- xts( stat.test,
                      order.by=test.end)

    P$fit.pars     <- c(fit.pars, P$fit.pars)
    P$stat.train <- c(stat.train, P$stat.train)
    P$stat.test  <- c(stat.test, P$stat.test)
  }

  return(P)
}

#' Some Title
#' 
Benchmark <- function(
  type=c("Hold","Random","rand pctile"),
  strategy,
  pars,
  dates=NULL,
  details=F,
  portfolio=Test(strategy,pars,dates=dates,details=details)
){
  type=type[1]
  pos <- portfolio$pos
  if(type=="Hold") {
    #simulate buy-hold
    pos[] <- 1
  }
  else if(type=="Random") {
    t <- NROW(pos)
    long.bias <- sum(pos>0)/t
    short.bias <- sum(pos<0)/t
    pos[] <- 1 * (long.bias - short.bias)
  }
  else stop("other than hold benchmarks not supported yet.")

  b <- portfolio(name=type,positions=pos, trades=F, store=F)
  return(b)
}