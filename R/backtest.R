

#' Backtest
#' 
#' @export
Backtest <- function(...) {
  
  # evaluate all rules into model portfolios and 
  # reconcile them into single model portfolio
  
#   applyPositionSignals <- function(portfolio, rules) {return(orders)}
#   applyOrderSignals <- function(portfolio, rules) {return(orders)}
  btportfolio <- new("Portfolio") # otherwise initial portfolio object could be passed in via dots 
  mportfolio <- NULL
  for(r in ls_rules()) {
      p <- eval.rule(r)$portfolio
     
      mportfolio <- if (is.null(mportfolio)) p
      else
        .rbind.data.table(mportfolio, p, use.names=TRUE)
  }
  setkey(mportfolio, Instrument, Date)
    
  mp <- new("Portfolio", assets=mportfolio)
  
    # apply rules in an existing portfolio, generate orders from the position signal

  orders <- mp$assets[, OrderSize:=c(ifelse(is.null(btportfolio),
                                            Pos[1],
                                            Pos[1] - btportfolio$position(Instrument))
                              ,diff(Pos))
               , by=Instrument]
  orders <- orders[OrderSize!=0,][,list(Instrument, Date, OrderSize)]

  # execute orders -> book transactions
  
  execute <- function(orders, portfolio=NULL, market=OHLCV, algo="MOC") {
    # Get portfolio data - if doesn't exist, initialize with date of first signal
    if(is.null(portfolio)){
      initDate <- min(orders$Date)
      portfolio <- data.table(Date=initDate, Instrument=unique(orders$Instrument), Pos=numeric(1), key="Date")
    }

    # execution algorithm: market on close
    # take the price on a date following immediately the order date 
    # (example: roll backwards the price from Monday if the order date is Saturday.
    # fillDate and fill price will be Monday)
    orders.filled <- market[,FillDate:=Date][orders[,Date:=Date + 1], roll=-Inf][
      ,Price:=Close][
        ,TxnQty:=OrderSize]
    # remove orders yet to be filled in the future (having FillDate==NA)
    orders.filled <- orders.filled[!is.na(FillDate)]
    
    orders.filled <- orders.filled[,list(Instrument, FillDate, TxnQty, Price)]
    setnames(orders.filled, "FillDate", "Date")
    setkey(orders.filled, Instrument, Date)
    txns <- orders.filled[, TxnValue:= TxnQty * Price]
    return(txns)
  }
  
  txns <- execute(orders)

  # update portfolio positions with new transactions
  btportfolio$addTxns(txns)
  btportfolio$calcPL(market=OHLCV) #market=ohlc
  

  a <- new("Account",portfolios=list(btportfolio))
  summary <- list()
  summary$returns <- summary.returns(a$returns(), byIns=F)
  print(summary$returns)
  plot(a$performance())
  return(list(account=a, summary=summary))
}




runLength <- function(x) {
  (x) * unlist(lapply(rle(as.vector(x))$lengths, seq_len))
}


#' Number of bars since a condition has been met
#' 
#' @export
BarsSince <- function(x) runLength(!x)

#' Number of bars to the next TRUE value
#' 
#' @export
BarsTo <- function(x) {
  
  rle <- rle(x)
  unlist(mapply(function(value, length)
  {
    if(value) rep(0, length) else seq(from=length, to=1)
  }
                , value=rle$values
                , length=rle$lengths))
}

#' Remove excessive signals
#' 
#' returns 1 on the first occurence of "true" signal in x
#' then returns 0 until y is true even if there are "true" signals in x
#' @export
ExRem <- function(x,y=!x) {
  filter=FALSE
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  
  for (i in 1:length(x)) {
    if(filter) {
      if(x[i]) x[i] <- FALSE
      if(y[i]) filter <- FALSE
    }
    if(x[i]) filter <- TRUE
  }
  x
}

#' Remove excessive signals
#' 
#' works as a flip/flop device or "latch" (electronic/electric engineers will know what I mean
#' returns 1 from the first occurence of TRUE signal in x
#' until a TRUE occurs in y which resets the state back to zero
#' unil next TRUE is detected in x...  
#' this essentially reverts the process of ExRem - multiple signals are back again
#' TEST : fill(c(1,1,0,1),c(1,0,0,0))
#' @export
Fill <- function(x,y=!x) {
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  latch <- FALSE
  for (i in 1:length(x)) {
    if(x[i]) latch <- TRUE
    if(y[i]) latch <- FALSE
    if(latch) x[i] <- TRUE
    #     if(y[i]) latch <- FALSE # include also this line in a variant where x=T where y=T
  }
  x
}

#' Remove excessive signals
#' 
#' Gives a "1" or true on the day that x crosses above y Otherwise the result is "0".
#' To find out when x crosses below y, use the formula Cross(y, x) 
#' @export
Cross <- function(x, y) {
  above <- x > y
  #below <- y < x
  ExRem(above)
}

anticipate <- function(x, k=1, pad=NA) {
  k <- abs(k)
  c( tail(x, -k), rep(pad, k) )
}

delay <- function(x, k=1, pad=NA) {
  k <- abs(k)
  c( rep(pad, k) , head(x, -k) )
}

shift <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  if(k>0) {
    return( c(rep(NA, k), x)[1 : length(x)] )
  }
  else if(k<0) {
    return( c(x[(-k+1):length(x)], rep(NA, -k)) )
  }
  else if(k==0)
    return(x)
  
}

deconstruct_and_eval2 = function(expr, envir = parent.frame(), enclos = parent.frame()) {
  
  if (!mode(expr) %in% c("call", "expression")) 
    return(expr)
  
  if (length(expr) == 1) {
    if (is.call(expr[[1]])) return (deconstruct_and_eval2(expr[[1]]))
    else return(expr)
  }
  # don't evaluate eval's if the environment is specified
  
  if (expr[[1]] == quote(eval) && length(expr) < 3) {
    return(deconstruct_and_eval2(eval(expr[[2]], envir, enclos), envir, enclos))
  }
  
  lapply(expr, function(m) {
    if (is.call(m)) {
      if (m[[1]] == quote(eval)) eval(m[[2]], envir, enclos)
      else deconstruct_and_eval2(m, envir, enclos)
    } else {
      
      # my edit to the [.data.table original
      if(exists(as.character(m),envir=as.environment(.GlobalEnv))) { #was R
        if(!is.function(eval(m, envir=as.environment(.GlobalEnv)))) #was R
          eval(m,envir=as.environment(.GlobalEnv)) #was R
        else
          m
      }
      if(exists(as.character(m),envir=.GlobalEnv)) {
        if(!existsFunction(as.character(m)))
          eval(m)
        else
          m
      } else
        # end edit
        m
    }
  })
}

construct = function(l) {
  if (length(l) == 0) return(NULL)
  if (length(l) == 1) return(l)
  
  if (identical(l[[1]], quote(`function`))) return(as.call(list(l[[1]], l[[2]], construct(l[[3]]))))
  
  if (!is.list(l)) return(l)
  
  as.call(setNames(lapply(l, function(m) {
    if (length(m) == 1) m
    else construct(m)
  }), names(l)))
}

#' Some Title
#' 
#' @export
Summary <- function
(portfolio, # portfolio object, having components R, pos and trades
 format=F,
 ... # other arguments passed to format.stats function
){
  c("curve", "trade", "period")
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
  
  
  
  return(out)
}
