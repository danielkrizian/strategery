# require(TTR)
# require(PerformanceAnalytics)
# require(plyr) # count function

#' Some Title
#' 
#' @export
summary.returns <- function(R, stats, as.rows=T, ...) {
  # table of statistics in rows and portfolios/models in columns
  if(is.null(colnames(R)))
    colnames(R) <- paste("m",1:NCOL(R), sep="")
  listR <- unclass(as.data.frame(R))
  ans <- sapply(listR,function(x,stats, ...) compute.stats(stat=stats,R=x, ...=...), stats=stats, ...=...)
  if(length(stats)==1) {
    ans <- t(as.matrix(ans))
    rownames(ans) <- stats
    colnames(ans) <- colnames(R)
  }
  ans
}

#' Look up statistic function
#' 
#' Search for statistic function by function name (incl. "compute." variant) 
#' or by name defined in statistics list
#' returns function
#' @export
stat.fun <- function(name) {
  l <- statistics
  label <- ifelse(is.null(l[[name]]), name, l[[name]][1])
  names(name) <- label
  
  o <- try(get(name), silent=TRUE)
  if(is.function(o)) return(name)
  
  compute.name <- paste("compute",name, sep=".")
  names(compute.name) <- label
  o <- try(get(compute.name), silent=TRUE)
  if(is.function(o)) return(compute.name)
  
  found.fun <- names(l[unlist(lapply(l, FUN=function(x) any(tolower(x)==tolower(name))))])
  if(length(found.fun)) {
    ret <- paste("compute",found.fun, sep=".")
    names(ret) <- capwords(name)
    return(ret)
  }
  else stop(paste("Couldn't find function",name,". Add it to the definitions table"))
}

#' Compute performance statistic
#' 
#' computes statistic, using function or name of the statistic supplied. 
#' TODO (Function intelligently looks up required arguments from the object slots. Some arguments are assumed, even if not provided with exact name: e.g. if slot 'ret' exists, it will be passed to the functions as 'x' argument)
#' stats - character vector of either function names or names of statistics (as defined)
#' ... - arguments passed to functions
#' names - (optional) names to apply to returned vector
#' returns named numeric vector of statistics
  
#' OPTIONAL TODO: common argument conversions: 
#' if supplied, convert 'ret' argument name to 'x'
#' if(all(pm==0)) # desired argument has not been supplied, 
#' so perform common conversions where applicable
#' names(args)[pmatch(names(args), 'R', nomatch = 0L) > 0L] <- 'x' #rename arg R to x
#' pm <- pmatch(names(args), onames, nomatch = 0L)
#' @export
compute.stat <- compute.stats <- function(stat, ..., name=NULL){
  single <- function(stat, ...){
    args<-list(...)
    fun <- stat.fun(name=stat)
    .formals <- formals(fun)
    onames <- names(.formals)
    pm <- pmatch(names(args), onames, nomatch = 0L) # find which arguments were called
    names(args[pm > 0L]) <- onames[pm] # discard redundant arguments which were not called
    .formals <- args[pm > 0L]
    .formals$... <- NULL
    val <- matrix(do.call(fun, .formals), nrow=1)
    rownames(val) <- ifelse(is.null(name),names(fun), "NULL")
    return(val)
  }

  out <- mapply(single, stat=stat, MoreArgs=list(...))
  out <- if(is.matrix(out)) t(out) else as.matrix(out)
  
  if(!is.null(name)) rownames(out) <- name
  return(out)
}

#' Some Title
#' 
#' @export
format.stats <- function(x, select.rows, select.cols, subset=NULL, sort=NULL, ascending=NULL, format='decimal', metrics.in.rows = TRUE, digits=2, row.names=rownames(x), col.names=colnames(x)) {
  # x - list, matrix or data frame of statistics. List will be converted to matrix
  # select.rows, select.cols - character vector specifying which rows/cols to only display as row names and col names 
  # subset - TODO. currently only select rows is enabled
  # metrics.in.rows - supports both tables with metrics in rows or in columns
  
  # data standardization
  if (is.list(x)) 
    x <- as.data.frame(unlist(x))
  
  # Select rows and columns, TODO subset data
  if(!missing(select.rows) | !missing(select.cols)) {
    if(missing(select.rows))
      select.rows <- rownames(x)
    if(missing(select.cols))
      select.cols <- colnames(x)
    x <- subset(x, rownames(x) %in% select.rows , select.cols )
  }

  x <- as.data.frame(x)
  colnames(x) <- NULL # coercion creates unwanted name x
  
  # Sort
  if (length(sort) && nrow(x)>1) {
    
    if(is.character(sort)) {
      if(is.null(ascending)) ascending<-FALSE
      row <- match(sort,rownames(x))
      if(is.na(row)) row <- match(sort,rownames(applyLabels(x)))
      sort.factors<- x[row,]
    }
    else if(identical(sort,row.names)) {
      if(is.null(ascending)) ascending<-TRUE
      sort.factors <- col.names(x)
    }
    if(is.logical(ascending)) ascending<-ifelse(ascending,1,-1)
    x<- x[,order(ascending*xtfrm(sort.factors))]
  }

  #   Values Formatting
  for(row in 1:NROW(x)) {
    val <- as.numeric(x[row,])
    val<- switch(rownames(x)[row],
                 
                 #### 3.10%
                 'CAGR'=,'Annualized Return'=,
                 'Volatility'=,'Annualized StDev'=,
                 'Average Trade'=,
                 'Average Win'=,
                 'Average Loss'=,
                 'Best Trade'=,
                 'Worst Trade'=,
                 'Average Winning Month'=,   
                 'Average Losing Month'=,
                 'Best Month'=, 
                 'Worst Month'=,    
                 'Best Year'=,   
                 'Worst Year'=,
                 'Expectancy'=sprintf("% .2f%%",val*100),
                 
                 #### -0.10%
                 'Max Daily Drawdown'=,'Max Drawdown'=,'Max DD'=,
                 'Average Drawdown'=,
                 'VaR.5%'=,
                 'CVaR'=sprintf("% .2f%%",-abs(val)*100),
                 
                 #### 52%
                 'Total Return'=,
                 'Time In Market'=,'Exposure'=,
                 'Trade Winning %'=,
                 '% Winning Months'=,
                 '% Winning Years'=,
                 'Positive 12 Month Periods'=sprintf("%.0f%%",val*100),
                 
                 #### 6.26
                 'Sharpe'=,
                 'DVR'=,
                 'MAR'=,
                 'Skew'=,
                 'Kurt'=,
                 'Win/Loss Ratio'=,
                 'Profit Factor'=,
                 CAGR.MaxDD=,
                 Min.Eq=,
                 U.Capture=,
                 D.Capture=,
                 U.Number=,
                 D.Number=,
                 U.Pctge=,
                 D.Pctge=,
                 p=round(val,2),
                 
                 #### 3.1
                 'Avg Drawdown Length'=,
                 'Avg Trades Per Year'=,
                 'Avg Days In Trade'=round(val,1),
                 
                 #### 31
                 'Trades'=round(val,0),
                 
                 #### Date Range
                 'Time Period'=val,
                 
{ #default format option
  if(format=='percent') sprintf(fmt=paste("%.",digits,"f%%", sep=""), val*100) else round(val,digits)
})
    x[row,] <-val
  }
  
  if(!metrics.in.rows) x <- t.data.frame(x)
  
  # metric labels
  applyLabels <- function(x) {
    replace <- c(
      CAGR.MaxDD = 'CAGR/MaxDD',
      Trades.In.Year='Trades/Yr',
      Win.Trades.Pct='Win Trades'
    )
    x <- rename(x, replace=replace)
    colnames(x) <- gsub('.'," ", colnames(x),fixed=TRUE)
    x
  }
  if(!identical(col.names,colnames(x))) {
    x <- applyLabels(x)
    colnames(x) <- col.names
  }
  if(!identical(row.names,rownames(x))) 
    rownames(x) <- row.names
  return(x)
}

#' Some Title
#' 
#' @export
Summary <- function
(portfolio, # portfolio object, having components R, pos and trades
 stats=c("curve", "trade", "period"),
 format=F,
 ... # other arguments passed to format.stats function
){
  if(identical(stats,c("curve", "trade", "period")))
    stats <- stats[1]
  if(length(stats)==1)
    stats <- switch(stats, 
                    curve=
                      c('Total Return','CAGR','Sharpe','Sortino','Volatility','DVR','MAR','Max Daily Drawdown','Average Drawdown','Avg Drawdown Length','Avg Trades Per Year'),
                    trade=
                      c('Trade Winning %','Average Trade','Average Win','Average Loss','W/L Ratio','Best Trade','Worst Trade','Avg Days in Trade','Expectancy','Profit Factor'),
                    period=
                      c('Time In Market','% Winning Months','Average Winning Month','Average Losing Month','Best Month','Worst Month','% Winning Years','Best Year','Worst Year','Positive 12 Month Periods'),
                    stats)
  
  if(is.null(portfolio$R))
    portfolio$R <- Returns(portfolio)
  if(is.null(portfolio$trades))
    portfolio$trades <- Trades(portfolio)
  
  out <- compute.stats(stat=stats,
                       R=portfolio$R, 
                       pos=portfolio$pos, 
                       trades=portfolio$trades)
  if(format)
    out <- format.stats(out, ...)
    colnames(out) <- portfolio$name
    return(out)
}

#' Some Title
#' 
#' top.n - vector. First element is lowest number of values. Second 
#' @export
Analyze.trades <- function(x,top.n=5, decreasing=F) {
  decreasing <- ifelse(decreasing,-1,1)
  return (x$trades[order(decreasing * x$trades$return)[1:top.n],])
}


#' Some Title
#' 
#' top.n - vector. First element is lowest number of values. Second 
#' @export
Analyze.years <- function(R,top.n=5) {
  
  data <- compute.annual.returns (R=b$R)
  years <- year(index(data))
  data <- coredata(data)
  rownames(data)<- years
  o <- order(data, decreasing=T)
  
  if(length(top.n)<2) top.n <- rep(top.n,2)
  top <- data[head(o,top.n[1]),]
  bottom <- data[tail(o,top.n[2]),]
  bottom <- bottom[order(bottom,decreasing=F)]
  list(top=top,bottom=bottom)
}

#' Some Title
#' 
#' top.n - vector. First element is lowest number of values. Second 
#' @export
Analyze.drawdowns <- function(equity=Equity(R=R,nas=F,drawdown=T), R=NULL, top.n=5) {
  
    x = checkData(R[, 1, drop = FALSE], method = "matrix")
  drawdowns = as.matrix(equity$drawdown)
  draw = c()
  begin = c()
  end = c()
  length = c(0)
  trough = c(0)
  index = 1
  if (drawdowns[1] >= 0) 
    priorSign = 1
  else priorSign = 0
  from = 1
  sofar = drawdowns[1]
  to = 1
  dmin = 1
  for (i in 1:length(drawdowns)) {
    thisSign <- ifelse(drawdowns[i] < 0, 0, 1)
    if (thisSign == priorSign) {
      if (drawdowns[i] < sofar) {
        sofar = drawdowns[i]
        dmin = i
      }
      to = i + 1
    }
    else {
      draw[index] = sofar
      begin[index] = from
      trough[index] = dmin
      end[index] = to
      from = i
      sofar = drawdowns[i]
      to = i + 1
      dmin = i
      index = index + 1
      priorSign = thisSign
    }
  }
  draw[index] = sofar
  begin[index] = from
  trough[index] = dmin
  end[index] = to
  if(from[1]==1 & to[1]==length(x)+1) len = 0 else len = (end - begin + 1)

    out <- list(return = draw, from = begin, trough = trough, to = end, 
                length = len, peaktotrough = (trough - 
                                                begin + 1), recovery = (end - trough))
    
    index.sorted <- sort(out$return, index = T)$ix[1:top.n]
    out <- lapply(out, function(x) x <- x[index.sorted])
    
    table <- data.frame("From"=time(R)[out$from], 
                        "Trough"=time(R)[out$trough], 
                        "To"=time(R)[out$to], 
                        "Depth"=base::round(out$return, 4), 
                        "Length"=out$length, 
                        "To Trough"=out$peaktotrough,
                        "Recovery"=ifelse(is.na(time(R)[out$to]), NA, out$recovery))
  colnames(table) = c("From", "Trough", "To", "Depth", "Length", "To Trough", "Recovery")
    
    return(table)
}


###### STATISTICS VALIDITY CHECKS #######

# Sharpe
# at least 36 months of data

# Profit Factor
# > 2
# Examining the 2:1 profit/loss requirement, you get the sense that strategies that feature a mountain of small bites are much more succeptible to changes in the market.
# For instance, if you have a strategy that states "90% of the time, with these conditions, it will reach 5 ticks before it reaches 25 ticks against."
# In theory, with commission and burden, at a 5/25 (or 1/5 PL ratio), you'd need to win about 6/7 or 86% of the time.
# When looking at this closer, if you've observed a 90% win rate and you're averaging a certain yield/time, it only takes a couple of losers in a row to REALLY kick you in the crotch. Essentially, you have to drop from 90% to 86% and now you're under water. Not only that, it only takes 2 or 3 consecutive losers to put you in the hospital.
# Conversely, a strategy that features a 2:1 PL ratio, and has a 50% win rate, it takes several losers in a row to create a knockout punch, which usually gives the trader enough time to pause, adjust, etc.

# RINA: 
# "good" RINA index is one over 100 and an acceptible ratio is between 30-100 and anything less than 30 (even negative) is not so good.

###### MAP OF STATISTICS #############

statistics <- list()
statistics[['cagr']] <- c("CAGR","Annualized Return")
# statistics[['excess']] <- c("Excess Return")
statistics[['twr']] <- c("TWR")
statistics[['total.return']] <- c('Total Return')
statistics[['sigma']] <- c("Volatility","Annualized StDev")
statistics[['max.drawdown']] <- c("Max Drawdown","Max Daily Drawdown","Max DD")
statistics[['avg.drawdown']] <- c('Average Drawdown')
statistics[['avg.drawdown.length']] <- c('Avg Drawdown Length')
statistics[["sharpe"]] <- c("Sharpe", "Sharpe Ratio")
statistics[["sortino"]] <- c("Sortino", "Sortino Ratio")
statistics[["dvr"]] <- c("DVR")
statistics[["mar"]] <- c("MAR")
statistics[["trades.per.year"]] <- c('Trades Per Year','Avg Trades Per Year')
statistics[['percent.in.market']] <- c('Time In Market','Percent In Market','Exposure')

statistics[['win.rate']] <- c("Win Rate","Hit Ratio", "Trade Winning %")
statistics[['avg.pnl']] <-
  c("Average Trade","Average P&L","Avg P&L","Average Trade P&L", "Avg Trade P&L", "Avg Trade")
statistics[['win.avg.pnl']]<-c("Average Win")
statistics[['loss.avg.pnl']]<-c("Average Loss")
statistics[['win.loss.ratio']]<-c("Win/Loss Ratio", "W/L Ratio")
statistics[['best.trade']]<-c("Best Trade")
statistics[['worst.trade']]<-c("Worst Trade")
statistics[['avg.days.in.trade']]<-c('Avg Days in Trade')
statistics[["ntrades"]] <- c("Trades")
statistics[["expectancy"]] <- c("Expectancy")
statistics[["profit.factor"]] <- c("Profit Factor")

statistics[["win.months.rate"]] <- c('% Winning Months')
statistics[["win.month.avg.return"]] <- c('Average Winning Month')
statistics[["loss.month.avg.return"]] <- c('Average Losing Month')
statistics[["best.month"]] <- c('Best Month')
statistics[["worst.month"]] <- c('Worst Month')

statistics[["win.years.rate"]] <- c('% Winning Years')
statistics[["best.year"]] <- c('Best Year')
statistics[["worst.year"]] <- c('Worst Year')
statistics[["win.12m.rate"]] <- c('Positive 12 Month Periods')
statistics[["time.period"]] <- c('Time Period')

# out$win.len = mean( tlen[ tpnl > 0 ])
# out$loss.len = mean( tlen[ tpnl < 0 ])
#   out$Win.Percent.Day = sum(bt$ret > 0, na.rm = T) / len(bt$ret)
#   out$Best.Day = bt$best
#   out$Worst.Day = bt$worst
 
#   'Skew'=,
#   'Kurt'=,
#   CAGR.MaxDD=,

#   U.Capture=,
#   D.Capture=,
#   U.Number=,
#   D.Number=,
#   U.Pctge=,
#   D.Pctge=,


##### FUNCTIONS OF STATISTICS #####
# input: equity or R (returns)
# R must have dates


#' Some Title
#' 
#' @export
compute.cagr <- function(R, equity=cumprod(1 + R)) {
  as.double( last(equity,1)^(1/compute.nyears(equity)) - 1 )
}

#' Some Title
#' 
#' @export
compute.premium <- function(
  Ra,
  Rb
){  
  return( compute.cagr(R=Ra) - compute.cagr(R=Rb) )
}

#' Some Title
#' 
#' @export
compute.edge <- function(
  strategy,
  pars,
  dates=NULL,
  portfolio=Test(strategy,pars, dates=dates, details=F, returns=F)
){
  rObs <- Returns(portfolio, type="periods", reduce=T, refresh=F)
  n <- NROW(rObs)
  if(n<2)
    return(0.5)
  rBench = Returns(portfolio=Benchmark(type="Random", portfolio=portfolio),reduce=F)
  muObs <- mean(rObs,na.rm=T)
  mu <- mean(rBench,na.rm=T)
  s <- sd(rObs, na.rm=T)
  statistic = (muObs - mu) / (s / sqrt(n))
  p.value <- pnorm(-statistic)
  return(p.value)
}

#' Some Title
#' 
#' @export
compute.total.return <- function (
  equity=if(continuous) cumsum(R) else cumprod(1 + R),
  R=NULL, 
  continuous=F
){
  equity = as.vector(coredata(equity))
  if(continuous)
    return(exp(last(equity))-1)
  else {
    return( last(equity) / first(equity) - 1 )
  }
}

#' Some Title
#' 
#' @export
compute.raw.annual.factor = function(x) {
  round( nrow(x) / compute.nyears(x) )
}

#' Some Title
#' 
#' @export
compute.annual.factor = function(x) {
  # 252 - days, 52 - weeks, 26 - biweeks, 12-months, 6,4,3,2,1
  possible.values = c(252,52,26,13,12,6,4,3,2,1)
  index = which.min(abs( compute.raw.annual.factor(x) - possible.values ))
  round( possible.values[index] )
}

#' Some Title
#' 
#' @export
compute.twr <- function(equity=cumprod(1 + R), R=NULL) 
  return(compute.total.return(equity))

#' Some Title
#' 
#' @export
compute.sigma <- function(R) {
  f = compute.annual.factor(R)
  x = as.vector(coredata(R))
  return( sqrt(f)*sd(x) )
}

#' Some Title
#' 
#' @export
compute.downside.deviation <- function (R, MAR = 0, method = c("full", "subset")) {
  method = method[1]
  R <- as.vector(R)
    if (!is.null(dim(MAR)))
      MAR = mean(checkData(MAR, method = "vector"))
    r = subset(R, R < MAR)
    switch(method, full = {
      len = length(R)
    }, subset = {
      len = length(r)
    })

#     if (!is.null(dim(MAR))) MAR = as.numeric(Return.annualized(MAR,geometric=FALSE))
#     MARlabel <- paste("Downside Deviation (MAR = ", round(MAR * 100, 1), "%)", sep = "")

    return(sqrt(sum((r - MAR)^2)/len))
}



#' Some Title
#' 
#' @export
compute.sharpe <- function(R) {
  f = compute.annual.factor(R)
  R = coredata(R)
  return(sqrt(f) * mean(R) / apply(R,2,sd) )
}

#' Some Title
#' TODO: currently supports only constant MAR
#' @export
compute.sortino <- function(R, MAR=0) {
  f = compute.annual.factor(R)
  R = coredata(R)
  
  if(is.null(dim(MAR)))
    excess.R <- R - MAR
  else (stop("MAR vector not yet supported"))

  MARlabel <- paste(round(mean(MAR) * 100, 3), "%)", sep = "")
  
  return( sqrt(f) * mean(excess.R) / compute.downside.deviation(R, MAR=MAR) )
}


#' Some Title
#' 
#' @export
compute.r2 <- function(equity=cumprod(1+R), R=NULL) {
  x = as.double(index(equity))
  y = as.double(equity)
  return( cor(y,x)^2 )
}

#' Some Title
#' 
#' @export
compute.dvr <- compute.DVR <- function(equity=cumprod(1+R), R)
  return( compute.sharpe(R) * compute.r2(equity) )

#' Some Title
#' 
#' @export
compute.mar <- function() NA

#' Some Title
#' 
#' @export
compute.kestner <- compute.kratio <- function(equity=cumprod(1 + R), R=NULL, adjust.n=F) {
  # adjust.n - divide by number of datapoints
  # adjust.n discussion (Zephyr K-ratio): 
  # http://s3.amazonaws.com/zanran_storage/www.styleadvisor.com/ContentPages/2449998087.pdf
  # formulas: http://www.financialwebring.org/gummy-stuff/K-Ratio.htm
  # amibroker implementation: http://www.mail-archive.com/amibroker@yahoogroups.com/msg39841.html
  n = nrow(equity)
  xt = 1:n
  log.e = coredata(log(equity))
  mod = summary(lm( log.e ~ xt ))
  slope = mod$coefficients['xt', 'Estimate']
  slope.stde = mod$coefficients['xt', 'Std. Error']
  k = slope/(slope.stde)
  if(adjust.n) k = k / n
  return(k)
}

#' Some Title
#' 
#' @export
compute.max.drawdown <- function(R=NULL, equity=Equity(R=R,nas=F,drawdown=T)) {
  as.double( min(equity$drawdown) )
}

#' Some Title
#' 
#' @export
compute.MCDD <- function(R=wf$R[wf$R!=0], conf=.95, samples=1000, parallel=F){
  require(np)
  require(boot)
  R <- R[R!=0]

  if(any(is.na(R)))
    stop("Clean NAs in R input in compute.MCDD")

  if(parallel){
    parallel="snow"
    ncpus=detectCores()
    cl=createCluster(parVar=c("Equity","string.range"), packages="xts")
  } else {
    parallel="no"
    ncpus=1
    cl=NULL
  }
  block.length <- b.star(R,round=T)[1] # for "geom", mean of blocks dist.
  
  bootstrap <- tsboot(tseries=R
             ,statistic=compute.max.drawdown
             ,R=samples
             ,sim="geom"  #"geom"
             ,l=block.length
             ,parallel=parallel
             ,ncpus=ncpus
             ,cl=cl)
  MCDD <- quantile(bootstrap$t, probs=1-conf)
  names(MCDD) <- NULL
  return (MCDD)
}


#' Some Title
#' 
#' @export
compute.avg.drawdown <- function(equity=Equity(R=R,nas=F,drawdown=T), R=NULL) {
  drawdown = rbind( coredata(equity$drawdown), 0 )
  dstart = which( drawdown == 0 & c(0,drawdown[-length(drawdown)]) != 0 )
  dend = which(drawdown == 0 & c(drawdown[-1], 0) != 0 )
  mean(apply( cbind(dstart, dend), 1, function(x){ min( drawdown[ x[1]:x[2] ], na.rm=T) } ))
}

#' Some Title
#' 
#' @export
compute.avg.drawdown.length <- function(equity=Equity(R=R,nas=F,drawdown=T), R=NULL) {
  drawdown = rbind( coredata(equity$drawdown), 0 )
  dstart = which( drawdown == 0 & c(drawdown[-1], 0) != 0 )
  dend = which(drawdown == 0 & c(0,drawdown[-length(drawdown)]) != 0 )
  return( mean(dend-dstart) )
}

#' Some Title
#' 
#' @export
compute.nyears <- function(R) {
  as.double(diff(as.Date(range(index(R)))))/365
}

#' Some Title
#' 
#' @export
compute.percent.in.market <- function(pos=bt$pos) {
  # Calculates percentage time the strategy signals position in the market.
  # Args:
  #   pos: position matrix, where columns are symbols, rows are positions for the following period, coded as one of the values: {NA, -1, 0, 1}
  # Returns:
  #   percentage of observations that are non-zero and non-NA. Columnnames are symbols.
  if(ncol(pos)>1) stop("compute.percent.in.market not ready for multi-column b$pos")
  sum(pos != 0, na.rm=TRUE) / nrow(pos)
}

#' Some Title
#' 
#' @export
compute.rina <- function(R) {
  # TODO:
  #   http://www.bigmiketrading.com/psychology-money-management/11594-evaluation-discussion-performance-ratios.html#post129146
  #   total net profit, divides it by the average drawdown and divides it again by the percent time in the market.
}

#' Some Title
#' 
#' @export
compute.ntrades <- function(trades=b$trades) {
  return(nrow(trades))
}

#' Some Title
#' 
#' @export
compute.win.rate <- function(trades=b$trades) {
  w <- as.double(trades[, 'size'])
  P.exit <- as.double(trades[, 'exit.price'])
  P.entry <- as.double(trades[,'entry.price'])
  pnl =  w * ( P.exit / P.entry - 1)
  return( sum( pnl > 0 ) / compute.ntrades(trades))
}

#' Some Title
#' 
#' @export
compute.pnl <- function(trades=b$trades) {
  w <- as.double(trades[, 'size'])
  P.exit <- as.double(trades[, 'exit.price'])
  P.entry <- as.double(trades[,'entry.price'])
  pnl =  w * ( P.exit / P.entry - 1)
  return( pnl )
}

#' Some Title
#' 
#' @export
compute.avg.pnl <- function(trades=bt$trades) {
  pnl =  compute.pnl(trades=trades)
  return( mean (pnl) )
}

#' Some Title
#' 
#' @export
compute.win.avg.pnl <- function(trades=bt$trades) {
  pnl =  compute.pnl(trades=trades)
  return( mean( pnl[pnl > 0] ) )
}

#' Some Title
#' 
#' @export
compute.loss.avg.pnl <- function(trades=bt$trades) {
  pnl =  compute.pnl(trades=trades)
  return( mean( pnl[pnl < 0] ) )
}

#' Some Title
#' 
#' @export
compute.win.loss.ratio <- function(trades=bt$trades) {
  return( abs (compute.win.avg.pnl(trades=trades) / compute.loss.avg.pnl(trades=trades) ) )
}

#' Some Title
#' 
#' @export
compute.best.trade <- function(trades=bt$trades) {
  return( max(as.double(trades[, 'return'])) / 100 )
}

#' Some Title
#' 
#' @export
compute.worst.trade <- function(trades=bt$trades) {
  return( min(as.double(trades[, 'return'])) / 100 )
}

#' Some Title
#' 
#' @export
compute.expectancy <- function(trades=bt$trades) {
  win.rate <- compute.win.rate(trades)
  avg.win <- compute.win.avg.pnl(trades)
  avg.loss <- compute.loss.avg.pnl(trades)
  return( win.rate * avg.win + (1-win.rate)* avg.loss )
}

#' Some Title
#' 
#' @export
compute.profit.factor <- function(trades=bt$trades) {
  win.rate <- compute.win.rate(trades)
  avg.win <- compute.win.avg.pnl(trades)
  avg.loss <- compute.loss.avg.pnl(trades)
  return( abs( avg.win / avg.loss * win.rate / (1- win.rate) ) )
}

#' Some Title
#' 
#' @export
compute.avg.days.in.trade <- function(trades=bt$trades) {
  days.in.trade = as.Date(trades[, 'exit.date']) - as.Date(trades[, 'entry.date'])
  return( mean(days.in.trade) )
}

#' Some Title
#' 
#' @export
compute.trades.per.year <- function(R, trades=bt$trades) {
  if(is.null(trades)) 
    stop("Missing trades. Couldn't calculate statistic: Trades Per Year")
  return(compute.ntrades(t=trades) / compute.nyears(R=R))
}

#' Some Title
#' 
#' @export
compute.monthly.returns <- function(equity=cumprod(1 + R), R=NULL){
  month.ends = unique(sort(c(1,endpoints(equity, 'months'))))
  return( ROC(equity[month.ends, ], type = 'discrete', na.pad=FALSE) )
}

#' Some Title
#' 
#' @export
compute.annual.returns <- compute.yearly.returns <- function(equity=cumprod(1 + R), R=NULL){
  year.ends = unique(sort(c(1,endpoints(equity, 'years'))))
  return( ROC(equity[year.ends, ], type = 'discrete', na.pad=FALSE) )
}

#' Some Title
#' 
#' @export
compute.rolling.returns <- function(R, window=12, na.rm=TRUE) {
  R <- apply.rolling(R, width=window, FUN=function(x){prod(1 + x) - 1})
  if(na.rm) R <- na.omit(R)
  return(R)
}

#' Some Title
#' 
#' @export
compute.win.months.rate <- function (equity=cumprod(1 + R), R=NULL) {
  mret <- compute.monthly.returns(equity)
  return( sum(mret >= 0, na.rm = T) / length(mret) )
}

#' Some Title
#' 
#' @export
compute.win.month.avg.return <- function(equity=cumprod(1 + R), R=NULL) {
  mret <- compute.monthly.returns(equity)
  return( mean(mret[mret > 0]) )
}

#' Some Title
#' 
#' @export
compute.loss.month.avg.return <- function(equity=cumprod(1 + R), R=NULL) {
  mret <- compute.monthly.returns(equity)
  return( mean(mret[mret < 0]) )
}

#' Some Title
#' 
#' @export
compute.best.month <- function(equity=cumprod(1 + R), R=NULL) {
  mret <- compute.monthly.returns(equity)
  return( max(mret, na.rm = T) )
}

#' Some Title
#' 
#' @export
compute.worst.month <- function(equity=cumprod(1 + R), R=NULL) {
  mret <- compute.monthly.returns(equity)
  return( min(mret, na.rm = T) )
}

#' Some Title
#' 
#' @export
compute.win.years.rate <- function (equity=cumprod(1 + R), R=NULL) {
  yret <- compute.yearly.returns(equity)
  return( sum(yret >= 0, na.rm = T) / length(yret) )
}

#' Some Title
#' 
#' @export
compute.best.year <- function(equity=cumprod(1 + R), R=NULL) {
  yret <- compute.yearly.returns(equity)
  return( max(yret, na.rm = T) )
}

#' Some Title
#' 
#' @export
compute.worst.year <- function(equity=cumprod(1 + R), R=NULL) {
  yret <- compute.yearly.returns(equity)
return( min(yret, na.rm = T) )
}

#' Some Title
#' 
#' @export
compute.win.12m.rate <- function (equity=cumprod(1 + R), R=NULL) {
  mret <- compute.monthly.returns(equity)
  ret.12m <- as.numeric(compute.rolling.returns(mret, window=12))
  return( sum( ret.12m > 0) / length( ret.12m ) )
}

#' Some Title
#' 
#' @export
compute.time.period <- function (equity=cumprod(1 + R), R=NULL) {
  temp <- Sys.getlocale("LC_TIME")
  Sys.setlocale(category="LC_TIME", locale="C")
  ret <- join( format( range(index(cumprod(1 + R))), '%b %Y'), ' - ')
  Sys.setlocale(category="LC_TIME", locale=temp)
  return(ret)
}

Return.annualized <- function (R, scale = NA, geometric = TRUE, ...) 
{
  # just added ... to the list of arguments to ensure compatibility with table.Arbitrary
  if (is.vector(R)) {
    R = checkData(R)
    R = na.omit(R)
    n = length(R)
    if (!xtsible(R) & is.na(scale)) 
      stop("'R' needs to be timeBased or xtsible, or scale must be specified.")
    if (is.na(scale)) {
      freq = periodicity(R)
      switch(freq$scale, minute = {
        stop("Data periodicity too high")
      }, hourly = {
        stop("Data periodicity too high")
      }, daily = {
        scale = 252
      }, weekly = {
        scale = 52
      }, monthly = {
        scale = 12
      }, quarterly = {
        scale = 4
      }, yearly = {
        scale = 1
      })
    }
    if (geometric) {
      result = prod(1 + R)^(scale/n) - 1
    }
    else {
      result = mean(R) * scale
    }
    result
  }
  else {
    R = checkData(R, method = "xts")
    result = apply(R, 2, Return.annualized, scale = scale, 
                   geometric = geometric)
    dim(result) = c(1, NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Annualized Return"
    return(result)
  }
}

pctDrawdown <- function (returns , geometric=TRUE, pct = 0.95, invert = TRUE, ...) {
  require(PerformanceAnalytics)
  DD  <- Drawdowns (returns, geometric=geometric)
  ret <- as.matrix( t ( abs( apply( DD , 2 ,quantile,probs= 1 - pct ,na.rm=TRUE) ) ) )
  if(!invert) ret <- -ret
  rownames(ret) <- paste(pct*100,"% Worst Drawdown", sep = "")
  ret
}

maxRecovery <- function(R, label = 'Longest recovery', ...) {
  
  y = checkData(R, method = "matrix")
  if(NCOL(y)==1) {
    result <- max(findDrawdowns(y)$length)
  } else {
    result <- aaply(y,2,function(x) max(findDrawdowns(x)$length) )
  }
  dim(result) <- c(1,NCOL(y))
  colnames(result) <- colnames(y)
  rownames(result) <- label
  return(result)
}

SharpeRatio.annualized <- function (R, Rf = 0, scale = NA, geometric = TRUE) 
{
  # customized from PerformanceAnalytics, because ...
  R = checkData(R)
  if (!is.null(dim(Rf))) 
    Rf = checkData(Rf)
  if (is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale, minute = {
      stop("Data periodicity too high")
    }, hourly = {
      stop("Data periodicity too high")
    }, daily = {
      scale = 252
    }, weekly = {
      scale = 52
    }, monthly = {
      scale = 12
    }, quarterly = {
      scale = 4
    }, yearly = {
      scale = 1
    })
  }
  sr <- function(R, Rf, scale) {
    xR = Return.excess(R, Rf)
    SR = Return.annualized(xR, scale = scale, geometric = geometric)/StdDev.annualized(R, 
                                                                                       scale = scale)
    SR
  }
  result = apply(R, 2, sr, Rf = Rf, scale = scale)
  dim(result) = c(1, NCOL(R))
  colnames(result) = colnames(R)
  # custom code
  rownames(result) = paste("Annualized Sharpe Ratio (Rf=", round(mean(Rf) * scale * 100, 1), "%)", sep = "")
  #     rownames(result) = paste("Sharpe Ratio (Rf=", round(mean(Rf,na.rm=TRUE) * scale * 100, 1), "%)", sep = "")
  # end of custom code
  return(result)
}

findDrawdowns <- function (R, geometric = TRUE, ...) 
{
  x = checkData(R[, 1, drop = FALSE], method = "matrix")
  drawdowns = Drawdowns(x, geometric = geometric)
  draw = c()
  begin = c()
  end = c()
  length = c(0)
  trough = c(0)
  index = 1
  if (drawdowns[1] >= 0) 
    priorSign = 1
  else priorSign = 0
  from = 1
  sofar = drawdowns[1]
  to = 1
  dmin = 1
  for (i in 1:length(drawdowns)) {
    thisSign <- ifelse(drawdowns[i] < 0, 0, 1)
    if (thisSign == priorSign) {
      if (drawdowns[i] < sofar) {
        sofar = drawdowns[i]
        dmin = i
      }
      to = i + 1
    }
    else {
      draw[index] = sofar
      begin[index] = from
      trough[index] = dmin
      end[index] = to
      from = i
      sofar = drawdowns[i]
      to = i + 1
      dmin = i
      index = index + 1
      priorSign = thisSign
    }
  }
  draw[index] = sofar
  begin[index] = from
  trough[index] = dmin
  end[index] = to
  # custom code
  if(from[1]==1 & to[1]==length(x)+1) len = 0 else len = (end - begin + 1)
  # end of custom code
  list(return = draw, from = begin, trough = trough, to = end, 
       length = len, peaktotrough = (trough - 
         begin + 1), recovery = (end - trough))
  
}

##### EXPERIMENTAL #####
# add.metric(compute.total.return,name='Total Return')
# metrics$'Total Return'$FUN
# metrics$'Total Return'$name

add.metric <- function (calcFUN, name = NULL, indexnum = NULL, store = TRUE)
{
  if (exists('metrics')) 
    metrics <<- metrics
  else 
    metrics <-list()
  tmp_metric <- list()
  
  tmp_metric$name <- name
  tmp_metric$FUN <- calcFUN
  
  indexnum <- if (!is.null(indexnum)) {
    indexnum
  } else name
  
  class(tmp_metric) <- "metric"
  metrics[[indexnum]] <- tmp_metric
  if (store)
    assign("metrics", metrics, pos = .GlobalEnv)
  else return(tmp_metric)
  tmp_metric$name
}

msum = function(x, ...){
  # applying multiple functions to one object
  # https://stat.ethz.ch/pipermail/r-help/2011-February/267641.html
  #
  fun.names = sapply(lapply(substitute(list(...)), deparse)[-1], paste, collapse="")
  mthd<-list(...)
  if(!is.list(x)) x = list(x)
  res = t(sapply(x, function(y) sapply(mthd, function(m) do.call(m, list(y)) )))
  colnames(res) = fun.names
  rownames(res) = names(x)
  res
}