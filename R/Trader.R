moc = function(time, symbol, quantity, price, marketdata) {
  day_following_the_signal = time + 60*60*24
  md = last(marketdata[paste0("::", day_following_the_signal), symbol])
  list(time = index(md),
       instrument = symbol,
       qty = quantity, 
       price =as.numeric(md)
       )
}

Trader.initialize = function(prices, portfolios){
  if(!is.list(portfolios))
    portfolios = list(portfolios)
  
  lapply(portfolios, function(pobject) {self$portfolios[[pobject$name]] <- pobject})
  
  self$prices = prices
  self$orderbook = data.table(Portfolio=character(0), Instrument=character(0), 
                              Date=as.POSIXct(character(0)), Qty=numeric(0),
                              Type=character(0), Status=character(0))
  self$algos$moc = moc
  
}

Trader.add_orders = function(portfolios, time, symbols = names(order.size), qty = order.size, type=NULL){
  
  new_orders = data.table(Portfolio=portfolios, Instrument=symbols, 
                          Date=time, Qty=qty, Type="", Status="Open")
  self$orderbook = rbindlist(list(self$orderbook, new_orders))
}

Trader.execute = function(){
  
  self$orderbook[Status=="Open", c("Status"):={
    if(length(Portfolio)) {
      portf = self$portfolios[[Portfolio]]
      Map(function(i, t, q) {
        fill = self$algos$moc(symbol=i, time=t, quantity=q, marketdata=self$prices)
        portf$add_txns(fill$instrument, fill$time, fill$qty, fill$price)
      }, Instrument, Date, Qty)
      Status = "Filled"
      list(Status)
    }
  }, by=Portfolio]
}

#' Trader Class
#' 
#' @import xts
#' @import R6
#' @export
#' @examples
#' TODO: consider rlist with queue instead of data.table
trader <- function(prices, portfolios){
  Trader$new(prices, portfolios)
}

Trader <- R6::R6Class("Trader",
                      public = list(
                        orderbook = NA,
                        prices = NA,
                        portfolios = list(),
                        algos = list(),
                        initialize = Trader.initialize,
                        add_orders = Trader.add_orders,
                        execute = Trader.execute
                      )
)
