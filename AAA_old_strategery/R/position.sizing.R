##### POSITION SIZING (WEIGHTS) ###############

psLeverage <- function(leverage=1){
  leverage
}


#' Some Title
#' 
#' @export
Weights <- function(R=data$prices, allocation=c(1,'equal', list(SPX=0.5, DAX=0.5)), rebalancing='daily'){
  # allocation: one of: 1. list of weights per symbol, in the form symbol=weight,
  #                     2. integer, for special case of single symbol composite,
  #                     3. 'equal' - weights are equal to 1/n, where n is the count of non-NA values in R for each timestamp
  
  df <- R
  allocation <- allocation[1]
  if(allocation=='equal') { # count number of existing symbols each day and increase count over time as the symbols start trading
    assetcount <- apply.daily(R, FUN = function(x) sum(!is.na(x)) )
    assetcount <- cummax(assetcount) # correct the missing values in the middle of the period: i.e. count can only increase with time
    w <- 1 / ifelse(assetcount != 0, assetcount, Inf)
    for (symbol in 1:NCOL(R)) df[, symbol] <- w
  }
  else {
    w <- as.vector(unlist(allocation))
    #     for (timestamp in 1:NROW(R)) df[timestamp, ] <- w
    df <- xts(matrix(rep(w,NROW(R)),ncol=length(w), byrow=TRUE), as.Date(index(R)) )
  }
  
  # rebalancing
  on <- switch(rebalancing,
               d=,day=,days=,daily='days',
               w=,week=,weeks=,weekly='weeks',
               m=,month=,months=,monthly='months',
               'none')
  if(on!='none') {
    df.rebal <- R
    df.rebal[] <- NA
    period.ends = endpoints(R, on)
    period.ends = period.ends[period.ends > 0]
    df.rebal[period.ends, ] = df[period.ends, ]
    return(df.rebal)
  }
  return(df)
}

#' Some Title
#' 
#' @export
makeWeights <- function( R, equal=FALSE, temporal=TRUE, scheme=NULL, rebalancing='monthly', from='1900-01-01'){
  #temporal: increasing number of instruments over time as they begin trading
  f.w <- NULL
  R <- R[paste(from,"::",sep="")]
  if(!missing(rebalancing)) {
    weights <- as.vector(unlist(scheme))
    on <- switch(rebalancing,
                 # b=,bar=,bars=,
                 d=,day=,days=,daily='days',
                 w=,week=,weeks=,weekly='weeks',
                 m=,month=,months=,monthly='months')
    rebaldates <- unique(index(R[c(1,endpoints(R, on=on))]))
    rebalweights <- matrix(rep(weights,length(rebaldates)),ncol=length(weights), byrow=TRUE)
    f.w <- xts(rebalweights,rebaldates)
    colnames(f.w) <- colnames(scheme)
  }
  else if(!is.null(scheme)) f.w <- as.xts(as.data.frame(scheme),as.Date(from)) # no rebalancing - buy-hold
  
  if(equal && temporal) {
    assetcount <- apply.daily(R, FUN = function(x) sum(!is.na(x)) )
    assetcount<-cummax(assetcount) # correct the missing values in the middle of the period: i.e. count can only increase with time
    w <- 1 / ifelse(assetcount!=0,assetcount,Inf)
    w <- as.xts(unique.data.frame(w))
    
    for (c in 1:NCOL(R)) f.w <- cbind(f.w,w)
    colnames(f.w) <- colnames(R)
  }
  
  f.w
}

#' Some Title
#' 
#' @export
ntop <- function (data, topn = 1, dirMaxMin = TRUE) {
  # taken from Systematic Investor Toolbox
  # makes top n assets equal weight
  temp = coredata(data)
  for (i in 1:nrow(data)) {
    x = temp[i, ]
    o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
    index = which(!is.na(x))
    x[] = NA
    if (len(index) > 0) {
      n = min(topn, len(index))
      x[o[1:n]] = 1/n
    }
    temp[i, ] = x
  }
  temp[is.na(temp)] = 0
  out = data
  out[] = temp
  return(out)
}

#' Some Title
#' 
#' @export
FinancialIndex <- function(scheme, rebalancing='monthly',label='index') {
  
  # support for monthly rebalancing currently
  symbols <- names(scheme)
  weights <- as.vector(unlist(scheme))
  
  for(symbol in symbols) {
    if(!exists(symbol,envir=.GlobalEnv)) stop(paste('Load symbol',symbol))
    if(is.null(f.R)) f.R <- get(symbol) else f.R <- cbind(f.R,R$bars)
  }
  
  f.R <- makeReturnFrame(symbols,type='discrete')
  on <-switch(rebalancing, monthly= 'months')
  rebaldates <- index(f.R[c(1,endpoints(f.R, on=on))])
  rebalweights <- matrix(rep(weights,length(rebaldates)),ncol=length(weights), byrow=TRUE)
  w <- xts(rebalweights,rebaldates)
  mf.R <- apply.monthly(f.R,Return.cumulative,geometric=TRUE)
  #   Ri <- Return.rebalancing(f.R, weights=w, wealth.index=FALSE, contribution=FALSE,geometric=FALSE) #method='simple'
  Ri <- Return.portfolio(mf.R, weights=first(w), wealth.index=FALSE, contribution=FALSE,geometric=TRUE, method='simple') #method='simple'
  ret <- 100*cumprod(1+Ri)
  colnames(ret) <- label
  attr(ret, 'weights') <- scheme
  return(ret)
}

# allocation <- list(SPX=1)
# data$weight <- ttr.TFH() #*Weights(rebalancing='none')