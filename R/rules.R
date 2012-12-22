rule.hold <- function() {
  long[] <- 1
  sig <- long
  return(sig)
}

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

rule.TFH <- function(first.n.bds=3, last.n.bds=0, mom=20) {

  bd2fomc <- indicators$bd2fomc
  bd2hol <- indicators$bd2holNYSE
  close <- Cl (market[['SPX']])
  
  tom <- sort(as.vector(outer(indicators$eomNYSE, (-last.n.bds+1):(first.n.bds)-1, "+")))
  tom <- indicators$bdNYSE[tom[tom > 0 & tom <= length(indicators$bdNYSE)]]
  
  long[tom] <- 1
  price.momentum <- lag(momentum(close, n=mom, na.pad=F), na.pad=F)
  long <- (long | bd2fomc == 1 | bd2hol == 1) * (price.momentum > 0)
  
  sig <- long - short
  return(sig)
}