# Deposits + Withdrawals + Realized PL + Unrealized PL + Interest Income
Account.deposit=function(amount, date) {}
Account.withdraw=function(amount, date) {}

#' @include Portfolio.R
#' @import data.table
Account <- setRefClass("Account"
                       , contains="Portfolio"
                       , fields = list(portfolios=function(l) {
                                         # list of portfolios
                                         if(missing(l)) return(invisible(positions))
                                         else {
                                           if(length(l)==1) {
                                             positions <<- l[[1]]$positions
                                             txns <<- l[[1]]$txns
                                           } else {
                                             pool.Portfolio <- function(x,y) {}
                                             positions <<- pool.Portfolio(NULL,NULL) # stump
                                           }
                                         }
                                       },
                                       benchmarks="data.table")
                       , methods = list(deposit=Account.deposit,
                                        withdraw=Account.withdraw)
)