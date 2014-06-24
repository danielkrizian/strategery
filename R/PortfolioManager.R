#' @include Portfolio.R
PortfolioManager <- setRefClass("PortfolioManager",
                       fields=list(
                         events="Queue",
                         portfolio="Portfolio", # ?
                         signals="data.table" # ?
                       ),
                       methods=list(
                         
  reconSignals = function(new.signals) {
    if(nrow(signals[duplicated(signals)])) {
      
      print(signals[duplicated(signals)])
      browser()
    }
  },
  
  createOrders = function(){
    "Apply rules in an existing portfolio, generate orders from signals.
      Signal = difference between existing and model portfolio"
    orders = signals[, OrderSize:=c(Units[1] - portfolio$state[[Instrument]],
                                    diff.default(Units)), by=Instrument]
#     orders = signals[, OrderSize:=c(Units[1],diff.default(Units)), by=Instrument]
    orders[OrderSize!=0][,c("Units","check.state"):=NULL]
  }
  
                       )
)