#' Run the strategy
#' 
#' @export
#' @examples
#' run()
run <- function() {
  SIG = copy(OHLCV)
  LONG = SIG[, Signal:=Long*weight][Long > 0][, list(Instrument, Date, Signal)]
  NEUTRAL = SIG[Neutral > 0][,Signal:=0][, list(Instrument, Date, Signal)]
  SIG = setkey(rbindlist(list(LONG, NEUTRAL)), Date, Instrument)
  
  prices=copy(OHLCV)
  prices = dcast.data.table(prices, Date ~Instrument, value.var = "Close")
  prices = xts(prices[, -1, with=F], order.by=as.POSIXct(as.character(prices$Date)))
  
  p = portfolio("ABC", prices=prices, equity=100000)
  tr = trader(prices=prices, portfolios=p)
  
  .implement_signals <- function(time, symbols, signals) {
    # manager waits for next signal
    p$update_to(time - 1e-5)
    
    # manager reads signals, consults current portfolio, determines position sizing and issues orders
    order.size = p$sizing(signals, symbols, round=TRUE)
    order.size = order.size[order.size != 0]
    if(length(order.size))
      tr$add_orders(portfolios=rep(p$name, length(order.size)), 
                        time=rep(time, length(order.size)), 
                        symbols = names(order.size), 
                        qty = order.size)
    
    # trader executes open orders
    tr$execute()
    
    # back office does the portfolio accounting for the given day/bar 
    p$update_bar(time)
  }
  
  
  SIG[, invisible(.implement_signals(Date, Instrument, Signal)), by=Date]
  
  return(p)
}

