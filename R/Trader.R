#' @include OMS.R
Trader <- setRefClass("Trader",
                      fields=list(
                        market="data.table",
                        oms="OMS"
                      ),
                      methods=list(
  check = function(txns) {
    if(nrow(txns[duplicated(txns)])){
      print("Multiple transactions generated for the same instrument: ")
      print(txns[duplicated(txns)])
      print("Backtest stopped to prevent error in PortfolioAnalyst$calcPL. TODO: make it robust.")
      browser()
    } else txns
  },
  
  collectOrders = function(calendar, lag.days=1) {
    "On trading days, collect open orders generated during non-trading days or
    otherwise still open/unexecuted orders. 
    Make trading calendar available for preparation/bundling of orders"
    
    orders = oms$orders
    
    calendar[, TradingDate:=Date] # need to make non-key copy that persists after join
    orders = calendar[orders[,Date:=Date + lag.days], roll=-Inf]
    # bundle orders for the trader to execute on a given trading day
    orders = orders[, list(OrderSize=sum(OrderSize)), 
                    keyby=list(Instrument,TradingDate)]
    oms$orders <<- orders[, list(Instrument, TradingDate, OrderSize)]
  },
  
  executeOrders = function(algo="MOC", lag.days=1) {
    "Execute orders -> book transactions
  
       Parameters:
      lag.days default = 1 (at least the next day after their generation)"
    
    # Note that on certain official calendar trading days, 
    # markets were unexpectedly closed, e.g. 2001-09-11 or 1985-09-27 (Hurricane Gloria) in US 
    # Therefore, collect & bundle orders on actual market days
    collectOrders(calendar=market, lag.days=lag.days)
    orders.collected = oms$orders
    # Some orders could get cancelled/netted. Remove them
    orders.collected = orders.collected[OrderSize!=0]
    # Execution algorithm: market on close
    # Example: take the price on a date following immediately the order date.
    # Roll backwards the price from Monday if the order date is Saturday.
    # fillDate and fill price will be Monday.
    orders.filled = market[,FillDate:=Date][orders.collected][, Price:=Close][,TxnQty:=OrderSize]
    
    # remove orders yet to be filled in the future (having FillDate==NA)
    orders.filled = orders.filled[!is.na(FillDate)]
    
    orders.filled = orders.filled[,list(Instrument, FillDate, TxnQty, Price)]
    setnames(orders.filled, "FillDate", "Date")
    setkey(orders.filled, Instrument, Date)
    fills = orders.filled[, TxnValue:= TxnQty * Price]
    seetattr(fills, "event.type", "fill")
    events$push(fills)
    if(verbose) {
      message("Orders filled ")
      print(fills)
    }
    return(check(fills))
  }
                      )
)