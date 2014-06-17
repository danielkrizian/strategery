Trader <- setRefClass("Trader",
                      fields=list(
                      market="data.table"
                      ),
                       methods=list(
                         check = function(txns) {
                           if(nrow(txns[duplicated(txns)])){
                             print("Multiple transactions generated for the same instrument: ")
                             print(txns[duplicated(txns)])
                             print("Backtest stopped to prevent error in Portfolio$calcPL. TODO: make it robust.")
                             browser()
                           } else txns
                         },
                         execute = function(orders, algo="MOC", lag.days=1) {
  "Execute orders -> book transactions
  Parameters:
    lag.days default = 1 (at least the next day after their generation)"
  # Note that on certain official calendar trading days, 
  # markets were unexpectedly closed, e.g. 2001-09-11 or 1985-09-27 (Hurricane Gloria) in US 
  # Therefore, collect & bundle orders on actual market days
  orders.collected = collect.orders(orders, calendar=market, lag.days=lag.days)
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
  txns = orders.filled[, TxnValue:= TxnQty * Price]
  return(check(txns))
                         }
                       )
)

# on trading days, collect open orders generated during non-trading days or
# otherwise still open/unexecuted orders
# make trading calendar available. For preparation/bundling of orders
collect.orders <- function(orders, calendar, lag.days=1) {
  calendar[, TradingDate:=Date] # need to make non-key copy that persists after join
  orders = calendar[orders[,Date:=Date + lag.days], roll=-Inf]
  # bundle orders for the trader to execute on a given trading day
  orders = orders[, list(OrderSize=sum(OrderSize)), keyby=list(Instrument,TradingDate)]
  orders[, list(Instrument, TradingDate, OrderSize)]
}

