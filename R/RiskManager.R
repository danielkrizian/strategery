RiskManager <- setRefClass("RiskManager",
                   fields=list(
                     portfolio="Portfolio",
                     oms="OMS"
                   ),
                   methods = list(
                     calcExposures = function(){
                       
                     },
                     
                     sizeOrders = function(){
                       
                     }
                   )
)

#' % risk budget = loss * shares / equity
#' utilizes internal state variable Equity stored in the Portfolio object 
risk <- function(budget, loss) {
  shares = budget * Equity / loss
  return(shares)
}

weight <- function(scheme=c("equal","market")) {
  shares = Equity / ( N * Price ) # equal weight
  shares = Equity / (sum(Price)) # market weight
}

capital <- function() {
  
}