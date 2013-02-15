
#' Some Title
#' 
#' @export
rule.hold <- function() {
  long[] <- 1
  sig <- long
  return(sig)
}

#' Some Title
#' 
#' @export
rule.rand <- function
(wlong=0 # TODO: proportion of long signals, when signals are generated parametrically
 ,wshort=0 # TODO: proportion of short signals, when signals are generated synthetically
 ,serial=F # TODO: stationary series? maintains serial correlation
 ,sig=Signals(strategy,pars)
 ,strategy
 ,pars
){
  if(wlong+wshort>0) {
    sig <- market$returns
    t <- NROW(sig)
    sig[] <- sample(c(1,0,-1),size=t,replace=T,prob=c(wlong,1-wlong-wshort,wshort))
  } else {
    sig[] <- coredata(sig)[sample(NROW(sig)),]
  }
  
  permute.rule=NULL #rule function from which to generate signals and permute them. Alternatively, and for speed sake, series of signals to permute. If NULL, generate from distribution -1,0,1 with weights (nlong, 1- nlong - nshort, nshort)
  #     if(is.null(model$sig))
    #       model$sig <- Signals(permute.rule)
    return(sig)
}

#' Some Title
#' 
#' @export
rule.TFH <- function(first.n.bds=3, last.n.bds=0, mom=20) {

  bd2fomc <- indicators$bd2fomc
  bd2hol <- indicators$bd2holNYSE
  close <- Cl (market[['SPX']])

  long[TurnMonth(first.n.bds, last.n.bds, lag=-1)] <- 1
  price.momentum <- lag(momentum(close, n=mom, na.pad=F), na.pad=F)
  long <-  (long | bd2fomc == 1 | bd2hol == 1) *  (price.momentum > 0) 
  
  sig <- long - short
  return(sig)
}

rule.smacross <- function(n_fast=50, n_slow=200){
  close <- Cl (market[['SPX']])
  sma_fast <- SMA(close, n=n_fast)
  sma_slow <- SMA(close, n=n_slow)

  long <-  sma_fast > sma_slow
  short <- sma_fast < sma_slow

  sig <- na.omit(lag(long - short))
  return(sig)
}