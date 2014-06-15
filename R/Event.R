#' Event is base class providing an interface for all subsequent (inherited) 
#' events, that will trigger further events in the trading infrastructure.'
Event <- setRefClass("Event",
                     fields = list(
                       .type="character"
                     ),
                     methods = list(
                     )
)

#' Handles the event of receiving a new market update with corresponding bars.
MarketEvent <- setRefClass("Market", contains="Event",
                           fields = list(
                           ),
                           methods = list(
                             initialize=function(){
                               .type <<- "market"
                             }
                           )
)

#' A SignalEvent requires a ticker symbol, a timestamp for generation and a 
#' direction in order to advise a Portfolio object. Handles the event of sending
#' a Signal from a Strategy object. This is received by a Portfolio object and 
#' acted upon.
SignalEvent <- setRefClass("Signal", contains="Event",
                           fields = list(
                             symbol="character",
                             datetime="Date", #consider POSIX et al.
                             type="integer" # 1 for long, -1 for short. Consider factor
                           ),
                           methods = list(
                             initialize=function(symbol, datetime, type){
                               'Initialises the SignalEvent.
                               Parameters: symbol - The ticker symbol, e.g. "GOOG". datetime - The timestamp at which the signal was generated.
                               type - "LONG" or "SHORT".'
                               .type <<- "signal"
                               symbol <<- symbol
                               datetime <<- datetime
                               type <<- type
                             }
                           )
)

#' Handles the event of sending an Order to an execution system.
#' The order contains a symbol (e.g. GOOG), a type (market or limit),
#' quantity and a direction. The quantity is determined by the Portfolio constraints.
OrderEvent <- setRefClass("Order", contains="Event",
                          fields = list(
                            symbol="character",
                            qty="numeric",
                            dir="character", # consider factor
                            type="integer" # 1 for long, -1 for short. Consider factor
                          ),
                          methods = list(
                            initialize=function(symbol, type, qty, dir){
                              ' Initialises the order type, setting whether it is
                              a Market order ("MKT") or Limit order ("LMT"), has a quantity (integral) and its direction ("BUY" or "SELL").
                              Parameters: symbol - The instrument to trade. order_type - "MKT" or "LMT" for Market or Limit.
                              quantity - Non-negative integer for quantity. direction - BUY" or "SELL" for long or short.'
                              .type <<- "order"
                              symbol <<- symbol
                              qty <<- qty
                              dir <<- dir
                              type <<- type
                            },
                            print=function(){
                              'Outputs the values within the Order.'
                              sprintf("Order: Symbol=%s, Type=%s, Quantity=%s, Direction=%s",
                                      symbol, order_type, qty, dir)
                            }
                          )
)

#' The FillEvent is the Event with the greatest complexity. It contains 
#' a timestamp for when an order was filled, the symbol of the order and 
#' the exchange it was executed on, the quantity of shares transacted, 
#' the actual price of the purchase and the commission incurred.
#' Encapsulates the notion of a Filled Order, as returned
#' from a brokerage. Stores the quantity of an instrument
#' actually filled and at what price. In addition, stores
#' the commission of the trade from the brokerage.
FillEvent <- setRefClass("Fill", contains="Event",
                         fields = list(
                           timeindex="Date", # consider POSIX
                           symbol="character",
                           exchange="character",
                           qty="numeric",
                           dir="character", # consider factor
                           cost="numeric",
                           commission="numeric"
                         ),
                         methods = list(
                           initialize=function(timeindex, symbol, exchange, 
                                               qty, dir, cost, commission=NULL){
                             'Initialises the FillEvent object. 
Sets the symbol, exchange, quantity, direction, cost of fill  and an optional commission.
If commission is not provided, the Fill object will calculate it based 
on the trade size and Interactive Brokers fees. 
Parameters:
        timeindex - The bar-resolution when the order was filled.
        symbol - The instrument which was filled.
        exchange - The exchange where the order was filled.
        quantity - The filled quantity.
        direction - The direction of fill ("BUY" or "SELL")
        fill_cost - The holdings value in dollars.
        commission - An optional commission sent from IB.'
                             .type <<- "fill"
                             timeindex <<- timeindex
                             symbol <<- symbol
                             exchange <<- exchange
                             qty <<- qty
                             dir <<- dir
                             cost <<- cost
                             commission <<- if(missing(commission)){
                               calcCommissionIB()
                             } else {
                               commission
                             }
                             
                           },
                           calcCommissionIB=function(){
                             'Calculates the fees in USD of trading based on 
                              an Interactive Brokers fee structure for API. 
                              This does not include exchange or ECN fees.
                              Based on "US API Directed Orders":
                              https://www.interactivebrokers.com/en/index.php?f=commission&p=stocks2'
                             full_cost = 1.3
                             if(qty<=500){
                               full_cost = max(1.3, 0.013 * qty)
                             } else {
                               # Greater than 500
                               full_cost = max(1.3, 0.008 * qty)
                               full_cost = min(full_cost, 0.5 / 100.0 * qty * cost)
                             }
                             return(full_cost)
                           }
                         )
)