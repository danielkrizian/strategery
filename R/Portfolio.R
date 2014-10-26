Portfolio.initialize = function(name, prices, equity=0, 
                                as.of=min(index(prices))-1e-5) {
  if (!missing(name)) self$name <- name
  
  state = t(rep(0, NCOL(prices)))
  colnames(state) = colnames(prices)
  
  txns = data.table(Instrument=character(0), Date=as.POSIXct(character(0)), 
                    Qty=numeric(0), Price=numeric(0), Value=numeric(0),
                    Gross.Realized.PL=numeric(0))
  
  summary = t(c(rep(0, 5), equity))
  colnames(summary) = c("Long", "Short", "Net", "Gross", 
                        "Net.Trading.PL", "Equity")
  
  self$t = as.of
  self$pos = state
  self$pos_value = state
  self$pos_avg_cost = state
  self$txn_value = state
  self$gross_realized_pl = state
  self$gross_trading_pl = state
  self$prices = state
  self$summary = summary
  self$txns = txns
  self$history$gross_trading_pl = xts(state, order.by=as.of)
  self$history$summary = xts(summary, order.by=as.of)
  self$history$prices = prices
}

Portfolio.get_prices = function(time){
  xts_data = last(self$history$prices[paste0("::", time)])
  out = xts_data
  attr(out, "timestamp") = index(xts_data)
  return(out)
}

Portfolio.add_txns = function(symbol, t, qty, price){
  
  # Validation
  if (t <= self$t)
    stop("Cannot add backdated transaction.")

  # Update if needed
  self$update_to(t - 1e-5)
  
  # Position
  last_pos = self$pos[, symbol]
  pos = last_pos + qty
  self$pos[, symbol] = pos
  
  # Transaction value
  txn_value = qty * price
  self$txn_value[, symbol] = txn_value
  
  # Position average cost
  last_pos_avg_cost = self$pos_avg_cost[, symbol]
  if (pos == 0) { # liquidated position
    self$pos_avg_cost[, symbol] = 0
  } else
    if (abs(pos) > abs(last_pos)) {# scaled-in position

      self$pos_avg_cost[, symbol] = 
        (last_pos * last_pos_avg_cost + 
           ifelse(last_pos_avg_cost>=0, 1, -1) * txn_value) / pos
    }
  
  # Realized PL
  gross_realized_pl = if( abs(pos) > abs(last_pos) )
                               0                        # if scaled-in => no PL
                              else qty * (last_pos_avg_cost - price)
  self$gross_realized_pl[, symbol] = gross_realized_pl
  
  # Append transaction
  self$txns = rbindlist(list(self$txns, list(symbol, t, qty, price, txn_value, 
                                             gross_realized_pl)))
  
}

Portfolio.update_bar = function(now, 
                                new_price=self$get_prices(now)){
  if(now <= self$t)
    return(invisible())
  if(!length(index(new_price)))
    return(invisible())
  if(index(new_price) <= self$t )
    return(invisible())
    
  new_price = coredata(new_price)

  last_pos_value = self$pos_value
  pos_value = new_price * self$pos
  gross_trading_pl = pos_value - last_pos_value - self$txn_value
  
  summary = self$summary
  summary[, "Long"] = sum(pos_value[, pos_value > 0], na.rm=T)
  summary[, "Short"] = sum(pos_value[, pos_value < 0], na.rm=T)
  summary[, "Net"] = sum(pos_value, na.rm=T)
  summary[, "Gross"] = sum(abs(pos_value), na.rm=T)
  summary[, "Net.Trading.PL"] = sum(gross_trading_pl, na.rm=T)
  summary[, "Equity"] = summary[, "Equity"] + summary[, "Net.Trading.PL"]
  
  self$prices = new_price
  self$pos_value = pos_value
  self$txn_value[] = 0
  self$summary = summary
  self$history$summary = rbind.xts(self$history$summary, 
                                   xts(summary, order.by=now))
  self$history$gross_trading_pl = rbind.xts(self$history$gross_trading_pl, 
                                    xts(gross_trading_pl, order.by=now))
  self$t = copy(now)
}

Portfolio.update_to = function(to, 
                               new_prices=self$history$prices[
                                 paste0(self$t,"::", to)]){
  
  if(self$t <= to) {
    for (bar in 1:NROW(new_prices)) {
      xts_price = new_prices[bar]
      now = index(xts_price)
      self$update_bar(now, xts_price)
    }
  }
}

Portfolio.print = function(){
  cat("Portfolio:", self$name, "\n" , "as of:", format(self$t))
}

#' Keeps track of all positions with a profit and loss ("PnL").
#' 
#' TODO: add benchmarks
#' TODO: solve contract multipliers in Portfolio$add_txns
#' TODO: interim on-the-fly valuation (equity value), without saving - for instant exposures
#' TODO: period summary of "Realized.PL", "Unrealized.PL", "Gross.Trading.PL", "Txn.Fees" if needed
#' 
#' Constraints: 
#' Specifying portfolio constraints as functions of time and asset. 
#' Constraints are applied when determining `sizing` of order in terms of units
#' p$constraints$max = function(t, symbols) {
#'    c(DBC=0.002,IEF=0.004)[symbols]
#' }
#' p$constraints$min = function(t, symbols) {
#'   -c(DBC=0.002,IEF=0.004)[symbols]
#' }
#' @import xts
#' @import R6
#' @export
#' @examples
#' p = portfolio("ABC", prices=prices, equity=100000)
portfolio <- function(name, prices, equity=0, as.of=min(index(prices))-1e-5) {
  Portfolio$new(name, prices, equity, as.of)
}

Portfolio <- R6::R6Class("Portfolio",
                         public = list(
                           name = "character",
                           t=NA,
                           pos = NA,
                           txns = NA,
                           txn_value = NA,
                           pos_value = NA,
                           pos_avg_cost = NA,
                           gross_realized_pl = NA,
                           gross_trading_pl = NA,
                           prices = NA,
                           summary = NA,
                           history = list(),
                           test = function(a, b) print(paste(a, b)),
                           constraints = list(max=NA, 
                                              min=NA),
                           initialize = Portfolio.initialize,
                           add_txns = Portfolio.add_txns,
                           update_bar = Portfolio.update_bar,
                           update_to = Portfolio.update_to,
                           get_prices = Portfolio.get_prices,
                           equity = function(new_price=NULL) {
                             return( as.numeric(self$summary[, "Equity"]))
                           },
                           sizing = function(weights, symbols, new_price=NULL, round=TRUE) {
                             
                             price = if(missing(new_price)) 
                               self$prices[, symbols] else new_price[, symbols]
                             
                             current_position = self$pos[, symbols]
                             
                             # apply constraints
                             if(!identical(self$constraints$max, NA))
                               weights = pmin(self$constraints$max(self$t, symbols), weights)
                             if(!identical(self$constraints$min, NA))
                               weights = pmax(self$constraints$min(self$t, symbols), weights)
                             target_position = weights * self$equity(new_price=new_price) / price
                             if(round)
                               target_position = trunc(target_position)
                             
                             return(target_position - current_position)
                           },
                           print = Portfolio.print
                         )
)

