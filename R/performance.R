# principles: assume no NAs
# lowest level statistics should be all based on single asset
# vectorize multi-asset on higher level

# Performance <- setRefClass("Performance"
#                            , fields = list(data="data.table",
#                                            assets="character"
#                            )
#                            , methods = list(
#                              
#                              
#                              
#                            ))

# ' Construct price object
prices <- function(..., symbol=NULL,
                   date.col="Date", asset.col="Instrument", val.col="Price") {
  
  dots <- list(...)
  if(!is.null(symbol))
    for(i in 1:length(dots)){
      setattr(dots[[i]], "symbols", symbol[i])
    }
  
  .prices <- function(data, symbol) {

    symbol <- attr(data, "symbols")
    data <- as.data.table(data)
    
    if(asset.col %in% colnames(data)) {
      if(length(unique(data[,asset.col,with=F]))==1 & !anyDuplicated(data[,date.col,with=F])) {
        # single asset, includes asset.col
        return(data)
      }
    } else {
      # wide multi-asset or single without asset.col
      if(length(symbol)==1) {
        #single asset, doesn't include asset.col
        
        renamePriceCol <- function(x, prefer=NULL) {
          
          if (is.null(prefer)) {
            if (has.Price(x)) 
              prefer = "price"
            else if (has.Trade(x)) 
              prefer = "trade"
            else if (has.Cl(x)) 
              prefer = "close"
            else stop("subscript out of bounds, no price was discernible from the data")
          }
          if (!is.null(prefer)) {
            loc <- NULL
            switch(prefer, Op = , open = , Open = {
              loc <- has.Op(x, which = TRUE)
            }, Hi = , high = , High = {
              loc <- has.Hi(x, which = TRUE)
            }, Lo = , low = , Low = {
              loc <- has.Lo(x, which = TRUE)
            }, Cl = , close = , Close = {
              loc <- has.Cl(x, which = TRUE)
            }, Bid = , bid = {
              loc <- has.Bid(x, which = TRUE)
            }, Ask = , ask = , Offer = , offer = {
              loc <- has.Ask(x, which = TRUE)
            }, Mid = , mid = , Midpoint = , midpoint = {
              loc <- has.Mid(x, which = TRUE)
            }, Trade = , trade = {
              loc <- has.Trade(x, which = TRUE)
            }, Price = , price = {
              loc <- has.Price(x, which = TRUE)
            }, {
              loc <- grep(prefer, colnames(x))
            })
            if (!identical(loc, integer(0))) {
              new <- names(x)
              new[loc] <- "Price"
              setnames(x, old=names(x), new= new)
              return(new)
            }
            else stop("subscript out of bounds, no price was discernible from the data")
          }
        }
        renamePriceCol(data)
        
        data[, Instrument:=symbol]
        return(data)
      } else {
        # multi asset
        id.vars <- match("Date", names(data))
        measure.vars <- colnames(data)[!colnames(data) %in% date.col]
        require(reshape2)
        return( melt(data, id.vars=id.vars, measure.vars=measure.vars,
                     variable.name="Instrument", value.name="Price",
                     na.rm=TRUE, variable.factor=FALSE) )
      }
    }
  }
  
  out <- rbindlist(lapply(dots, .prices))
  
  new <- c("Instrument","Date", "Price")
  if(asset.col!="Instrument" | date.col!="Date" | val.col!="Price")
    setnames(out
             , old=c(asset.col,date.col, val.col)
             , new=new)
  setcolorder(out, c("Instrument", "Date", "Price", setdiff(names(out), new)))
  out[, Date:=as.IDate(Date)]
  setkey(out, Instrument, Date)
  out <- out[,new, with=F]
  setattr(out, "class", c("prices", class(out)))
  return(out)
}

#' Constructor for returns object
#' 
#' Header is Instrument, Date, Return. 
#' returns are in data.table with either single or multi-assets, either tall or wide format
#' @param symbol name for the instrument, if not supplied within ... object. 
#' @param ... Multi-instrument or single instrument data.
#' @param rf character list of  
#' Rf and benchmarks are stored as list(InstrumentName=Rf) or 
#' list(InstrumentName=Rb) pairs in the attributes
#' and their return data aligned by Date and rbinded for later retrieval
returns <- function(..., symbol=NULL, rf=NULL, bench=NULL, as.interval="irregular",
                    date.col="Date", asset.col="Instrument", val.col="Return") {
  # instruments tall = "Instrument" column or single-asset
  # instruments wide = 
  
  dots <- list(...)
  if(is.prices(dots[[1]]) & length(dots)==1)
    return(as.returns.prices(dots[[1]]))
  
  if(!is.null(symbol))
    for(i in 1:length(dots)){
      setattr(dots[[i]], "symbols", symbol[i])
    }
  
  .returns <- function(data, symbol) {
    symbols <- attr(data, "symbols")
    data <- as.data.table(data)
    if(asset.col %in% colnames(data)) {
#       if(length(unique(data[,asset.col,with=F]))==1 & !anyDuplicated(data[,date.col,with=F])) {
        # single asset, includes asset.col
        return(data)
#       } else {
        
#       }
      
    } else {
      # wide multi-asset or single without asset.col
      if(length(symbols)==1) {
        #single asset, doesn't include asset.col
        data[, Instrument:=symbols]
        return(data)
      } else {
        # multi asset
        id.vars <- match("Date", names(data))
        measure.vars <- colnames(data)[!colnames(data) %in% date.col]
        require(reshape2)
        return( melt(data, id.vars=id.vars, measure.vars=measure.vars,
                     variable.name="Instrument", value.name="Return",
                     na.rm=TRUE, variable.factor=FALSE) )
      }
    }
  }
  
  out <- rbindlist(lapply(dots, .returns))
  
  new <- c("Instrument","Date", "Return")
  if(asset.col!="Instrument" | date.col!="Date" | val.col!="Return")
    setnames(out
             , old=c(asset.col,date.col, val.col)
             , new=new)
  setcolorder(out, c("Instrument", "Date", "Return", setdiff(names(out), new)))
  
  if(!identical(as.interval,"irregular"))
    if(identical(as.interval,"monthly"))
      out[,Date:=as.yearmon(Date)]
  
  out[, Date:=as.IDate(Date)]
  setkey(out, Instrument, Date)
  out <- out[,new, with=F]
  class(out) <- c("returns", class(out))
  
  if(is.returns(bench)) {
    
    setkey(out, Date)
    setkey(bench, Date)
    out <- bench[out]
    setnames(out,
             c("Date" ,"Instrument",  "Return" ,"Instrument.1", "Return.1"),
             c("Date" ,"BM.Instrument",  "Benchmark" ,"Instrument", "Return"))
    out <- out[,list(Instrument, Date, Return, Benchmark, BM.Instrument)]
    setkey(out, Instrument, Date)
    
  }
  
  #   l <- as.data.table(list(A="b",C=c("d","e")))
  #   fun <-  function(i) data.table(Instrument=names(l)[[i]],
  #                                  Number=seq_along(l),
  #                                  Benchmark=l[[i]])
  #   setattr(out, "bmmap" , rbindlist(lapply(seq_along(l), fun )))
  
  out
}

  #' Coerce to prices class - time series of prices
  #' 
  #' @export
as.prices <- function(x, ...) {
  UseMethod("as.prices",x)
}

#' Coerce to returns class - time series of returns
#' 
#' @export
as.returns <- function(x, ...) {
  UseMethod("as.returns",x)
}

as.returns.prices <- function(x, ...) {
  
  x[, Return:=Price / c(NA,head(Price,-1)) - 1 , by=Instrument]
  x[, Price:=NULL]
  x <- x[!is.na(Return)]
  setattr(x, "class", c("returns", setdiff(class(x), "prices")))
  x
}

# as.prices.default <- function(x, interval) {
#   
#   columnsInput <- c("Name", "Date", "Value")
#   columnsOutput <- c("Name", "Date", "Value")
#   .key <- c("Name","Date")
#   
#   ans <- as.data.table(x)
#   
#   ans[, Date := as.IDate(Date)]
#   ans <- ans[, columnsInput, with=FALSE ]  # setcolorder(ans, newNames) probably not needed
#   setnames(ans, columnsOutput)
#   
#   if(!missing(interval))
#     ans <- to.interval(ans, to=interval)
#   
#   setkeyv(ans, .key)
#   setattr(ans, "class", c(attr(ans,"class"), "prices"))
#   #setattr(ans, "ohlc", ohlc)
#   return(ans)
# }


# 
# as.returns.prices <- function(x, interval=c("days","weeks","months","years")) {
#   
#   columnsInput <- c("Name", "Date", "Value")
#   columnsOutput <- c("Name", "Date", "Value")
#   .key <- c("Name","Date")
#   
#   ans <- x[, columnsInput, with=FALSE ]  # setcolorder(ans, newNames) probably not needed
#   setnames(ans, columnsOutput)
#   
#   if(missing(interval)) {
#     #compress to the lowest common interval
#     lci <- max(prices[,list("Interval"=periodicity(Date)$frequency),by=Name]$Interval)
#     interval <- as.units.frequency(lci)
#   }
#   
#   ans <- to.interval(ans, to=interval)
#   ans[!is.na(Value),Value:=roc(Value, na.pad=T), by=Name]
#   
#   # returns should be first, but problems with RStudio data viewer
#   setattr(ans, "class", c(attr(ans,"class"), "returns")) 
#   setattr(ans, "interval", interval)
#   return(ans)
# }


is.returns <- function(x) {
  inherits(x, "returns")
}

is.prices <- function(x) {
  inherits(x, "prices")
}

monthly <- function(x, ...) {
  UseMethod("monthly",x)
}

monthly.prices <- function(x) {
  x <- x[endpoints(Date, on="months")][,Date:=as.IDate(as.yearmon(Date))]
  setattr(x, "class", c("prices", class(x)))
  return(x)
}

monthly.returns <- function(x){
  
}



summary.returns <- function(x) {
  
#   Rf.bench attr()
  ann=252
  compound=T
  x[, Equity:= cumprod(1+Return), by=Instrument]
  x[, Drawdown:= Equity / cummax(Equity) - 1, by=Instrument]
  drawdowns <- x[, summary.drawdowns(Drawdown, Date), by=Instrument]
  return(x[,list(
    "CAGR"=annualized(Return, ann=ann, compound=compound)
    , "Total Return"=cumulative(Return, compound=compound)
    , "Sharpe"=sharpe(Return, Rf=0, ann=ann)
    , "Volatility"=sigma(Return, ann=ann)
    , "R2"=r2(cumprod(1+Return))
    , "DVR"= dvr(Return, Rf=0, ann=ann) # dvr(Return)
    , "MAR" = mar(Return, ann=ann)
    , "Max Drawdown"= maxdd(Return)
    , "Average Drawdown"= mean(drawdowns$Depth) #avgdd(Return)
    , "Average Drawdown Length" = mean(drawdowns$Length)
                 )
           , by=Instrument])
}

plot.returns <- function(x) {
  require(ggplot2);  require(ggthemes); require(grid); require(scales)
  x <- x[,list(Instrument,Date,Equity, Drawdown)]
  x <- data.table:::melt.data.table(x, id.vars=c("Instrument","Date"))
  
  p <- ggplot(x, aes(x=Date, y=value, colour=Instrument)) + geom_line() +
    #   scale_y_continuous(trans=log10_trans())
    # p + 
    facet_grid(variable ~ ., scales="free_y") +
    scale_x_date(breaks=pretty_breaks(n=10), minor_breaks="year", labels=date_format("%Y")) + # scale_x_date(breaks=number_ticks(7), labels=date_format("%Y"))
    scale_y_continuous(labels = percent_format()) +
    coord_trans(y="log1p") + 
    #
    theme_economist_white(gray_bg=FALSE) + 
    scale_colour_economist() +
    xlab("") + ylab("Cumulative Performance (Log scale)")
  
  g = ggplotGrob(p)
  panels = which(sapply(g[["heights"]], "attr", "unit") == "null")
  g[["heights"]][panels] = list(unit(12, "cm"), unit(3, "cm"))
  dev.off()
  grid.draw(g)
}


# #' Some Title
# #' 
# #' @export
# summary.returns <- function(R, stats, as.rows=T, ...) {
#   # table of statistics in rows and portfolios/models in columns
#   if(is.null(colnames(R)))
#     colnames(R) <- paste("m",1:NCOL(R), sep="")
#   listR <- unclass(as.data.frame(R))
#   ans <- sapply(listR,function(x,stats, ...) compute.stats(stat=stats,R=x, ...=...), stats=stats, ...=...)
#   if(length(stats)==1) {
#     ans <- t(as.matrix(ans))
#     rownames(ans) <- stats
#     colnames(ans) <- colnames(R)
#   }
#   ans
# }

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



# VALIDITY CHECKS FOR METRICS ---------------------------------------------

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

# MAP OF METRICS ----------------------------------------------------------

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



# GROWTH -----------------------------------------------------------------

#' Calculate coumpounded annualized growth rate
#' 
#' Missing values treatment: All leading NAs are removed (e.g. different 
#' inception dates of two aligned return series). Trailing NAs are removed (e.g.
#' instruments redeemed/sold from the portfolio, but aligned to the currently 
#' held instruments). Intradata NAs are NOT removed - i.e. return 
#' 
#' @export

compute.cagr <- function(
  R
  , equity=equity.index(R
                        , continuous=continuous
                        , base=1
                        , na.fill=list(0,0,0))
  , continuous=F
  , ann=ann.factor("monthly")
  , allow.incomplete=F
){
  n <- length(equity) - 1 # deduct one because equity contains base observation
  cagr <- if(!allow.incomplete & n < ann)
    as.numeric(NA)
  else
    ( last(equity) / first(equity) ) ^ (ann / n) - 1
  return(cagr)
}

#' Total compounded return over the period
#' 
#' @export
total.return <- period.return <- compute.total.return <- function (
  R
  , equity=equity.index(R, continuous=continuous, base=1, na.fill=list(0,0,0))
  , continuous=F
){
  result <- if(continuous)
    exp( last(equity) ) - 1
  else
    last(equity) / first(equity) - 1
  return(result)
}


#' Equity curve versus moving average (old mom)
#' 
#' @export
compute.momentum <- function(
  R
  ,equity=equity.index(R, continuous=F, base=1, na.fill=list(0,0,0))
  ,lag.n=0
  ,smooth.n=1
)
{
  if(length(equity) < smooth.n)
    return(as.numeric(NA))
  ma <- lag(SMA(equity, n=smooth.n),k=lag.n, na.pad=F)
  last(equity) / last(ma) - 1
}

#' Compute Market-Timing Premium
#' 
#' Excess return over long/short position bias adjusted market benchmark
#' @param R vector of strategy returns
#' @param pos vector of strategy positions
#' @param Rb vector of raw market returns
#' @export
compute.mean.excess.return <- function(
  R,
  pos,
  Rb
){  
  mean(R-Rb * pos)
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
compute.twr <- function(equity=cumprod(1 + R), R=NULL) 
  return(compute.total.return(equity))

# RISK --------------------------------------------------------------------

#' Some Title
#' 
#' @export
sigma <- volatility <- vol <- std <- stdev <- StDev <- compute.sigma <- function
( 
  R
  ,ann=ann.factor("monthly")
)
{
  sqrt(ann) * sd(R, na.rm=TRUE)
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

# RISK-ADJUSTED PERFORMANCE ------------------------------------------------------

#' Compute Sharpe Ratio
#' 
#' @param R vector of periodic returns (assumed monthly by default, but controlled
#' via 'ann' argument)
#' @export
sharpe <- compute.sharpe <- function 
( 
  R
  ,Rf=0
  ,ann=ann.factor("monthly")
)
{
  sqrt(ann) * mean(R - Rf) / sd(R)
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


# DRAWDOWN ANALYSIS -------------------------------------------------------

cumdrawdown <- function
(
  R=NULL
  ,equity=equity.index(R=R, continuous=F, base=1)
){
  equity / cummax(equity) - 1
}

#' Calculate Last Drawdown
#' 
#' @export
drawdown <- compute.drawdown <- function
(
  R=NULL
  ,equity=equity.index(R=R, continuous=F, base=1)
){
  last(equity) / max(equity) - 1
}

#' Some Title
#' 
#' @export
compute.max.drawdown <- function(R=NULL, equity=Equity(R=R,nas=F,drawdown=T)) {
  as.double( min(equity$drawdown) )
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

########## SOME SECTION ########

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

###### TRADES ######

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

##### PERIODS ######

period <- function(from, to, format="%b %y", sep=" - ") {
  paste(strftime(from, format=format)
        ,strftime(to, format=format)
        ,sep=sep)
}

# nperiods <- function(x, ann=NA) {
#   if(is.na(ann)) {
#     freq <- if(timeBased(x)) periodicity(as.POSIXct(x))$frequency
#     ann <- (60*60*24*31) / freq
#   }
#   nobs <- length(x)
#   if(nobs>1){
#     result <- floor( nobs / ann  )
#   } else result <- NA
#   as.integer(result)
# }

# # weeks
# difftime(strptime("26.03.2014", format = "%d.%m.%Y"),
#          strptime("14.01.2013", format = "%d.%m.%Y"),units="weeks")
# Time difference of 62.28571 weeks
# 
# # months
# (as.yearmon(strptime("26.03.2014", format = "%d.%m.%Y"))-
#    as.yearmon(strptime("14.01.2013", format = "%d.%m.%Y")))*12
# [1] 14
# 
# # quarters
# (as.yearqtr(strptime("26.03.2014", format = "%d.%m.%Y"))-
#    as.yearqtr(strptime("14.01.2013", format = "%d.%m.%Y")))*4
# [1] 4
# 
# # years
# year(strptime("26.03.2014", format = "%d.%m.%Y"))-
#   year(strptime("14.01.2013", format = "%d.%m.%Y"))
# [1] 1


nmonths <- function(from, to) {
  (as.yearmon(as.Date(to))-
     as.yearmon(as.Date(from)))*12
}

#' Some Title
#' Unfinished
#' @export
nyears <- function(x) {
  if(is.xts(x))
    x <- index(x)
  if(is.data.table(x))
    if(any("Date" %in% colnames(x)))
      x <- as.Date(x[,Date])
  x <- as.Date(x)
  
  add <- ifelse(periodicity(x)$scale=='monthly',as.POSIXlt(min(x))$mday,1)
  return((as.double(diff(range(x)))+add)/365)
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

# FIXES -------------------------------------------------------------------

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

# EXPERIMENTAL ------------------------------------------------------------

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