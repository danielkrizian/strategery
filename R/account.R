
Account <- setRefClass("Account"
                         , contains="Portfolio"
                         , fields = list(entries="data.table"
                                         ,portfolios="list"
                                         ,summary="data.table") 
# summary = dates ~ Deposits + Withdrawals + Realized PL + Unrealized PL + Interest Income
                         , methods = list(
                           
                           initialize=function(...)  {
                             assign('.performance',numeric(), .self)
                             .self$initFields(...)
                           },
                           
                           deposit=function(amount, date) {
                             
                           },
                           
                           withdraw=function(amount, date) {
                             
                           },
                           
                           markPL=function(){
                             
                           },
                           
                           value= function() {
                             
                             if(length(portfolios)==1) {
                               assets <<- portfolios[[1]]$assets
                               txns <<- portfolios[[1]]$txns
                               s(assets[,list(
                                 PL=sum(PL)), by=Date])
                               
                             } else {
                               pool.Portfolio <- function(x,y) {}
                               assets <<- pool.Portfolio(NULL,NULL) # stump
                             }

                           })
)

