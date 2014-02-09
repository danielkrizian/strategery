
##### ASSETS #######

Assets <- setRefClass("Assets"
                      , fields = list(name="character"
                                      , assets = "data.table"
                                      # function(v) {
#                                         # usually has fields Return, NAV
#                                         if(missing(v)) {
#                                           
#                                           if(inherits(.self,"Account")) {
#                                             .self$NAV()
#                                           }
#                                           
#                                           return(.performance)
#                                         }
#                                         assign('.performance',v,.self)
#                                       }
#                                       , prices="data.table" # to .perf
#                                       , returns="data.table" # to .perf
                                      , info="data.frame"
                      )
                      , methods = list(
                        
                        initialize=function(...)  {                        
                          assign('.performance', data.table(), .self)
                          .self$initFields(...)
                        },
                        
                        returns=function(interval="days") {

                          if(length(.performance))
                            performance <- .performance
                          else {
                            if(inherits(.self, "Portfolio")) {
                              if(! "PL" %in% names(assets)) assets <<- .self$calcPosPL()
                              
                              performance <- assets[,list(PL=sum(PL), Prev.Value=sum(Prev.Value)), by=Date]
                              performance[             , Return:=0]
                              performance[Prev.Value!=0, Return:=PL/Prev.Value]
                              performance[,Instrument:=if(length(name)) name else "Portfolio"]
                            }
                            if(inherits(.self, "Account") & !is.null(.self$benchmarks)) {
                              
                            }
                          }
                          
                          performance <- performance[,list(Instrument, Date, Return)] 
#                           setkey(performance, Instrument, Date) # "Date" key lost here
                          class(performance) <- c("returns", class(performance))
                          return(performance)
                        },
                        
                        prices=function(interval="days", base=100) {
                          if(length(.performance))
                            performance <- .performance
                          else {
                            performance <- returns(interval=interval)
                            if(inherits(.self, "Portfolio")) {
                              performance[, Index:=base*cumprod(1+Return)]
                            }
                          }
                          class(performance) <- c("prices", class(performance))
                          return(performance)
                        },
                        
                        performance=function() {
                          performance = .self$returns()
                          performance[,Equity:=cumprod(1+Return), by=Instrument]
                          performance <- performance[,Drawdown:=dd(Return), by=Instrument]
                          performance
                        })
)
##### PORTFOLIO #######

Portfolio <- setRefClass("Portfolio"
                         , contains="Assets"
                         , fields = list(txns="data.table"
                                         , exposures = "data.table" 
                         )
                         , methods = list(
                           
                           initialize=function(...)  {
                             assign('.performance',numeric(), .self)
                             .self$initFields(...)
                           },
                           
                           position = function(instrument=NULL, date=NULL){
                             if(!length(assets))
                               return(0)
                             last(assets[Instrument==instrument,]$Pos)
                           }, 
                           
                           addTxns = function(x){
                             # Update portfolio positions with new transactions
                             if(is.null(txns)) txns <<-x else {
                               txns <<- .rbind.data.table(txns, x, use.names=TRUE)
                               setkey(txns, Instrument, Date)
                             }
                             x[,Pos:=position(Instrument) + cumsum(TxnQty), by=Instrument]
                             
                             if(is.null(assets))
                               assets <<- x[,list(Instrument, Date, Pos)]
                             else
                               assets <<- .rbind.data.table(assets, x[,list(Instrument, Date, Pos)], use.names=TRUE)
                             setkey(assets, Instrument, Date)
                           },
                           
                           calcPL = function(market=OHLCV){
                             
                             #' Calculate portfolio profit & loss for each period
                             #' 
                             #' Gross.Trading.PL=Pos.Value- LagValue - Txn.Value
                             #' Period.Unrealized.PL = Gross.Trading.PL - Gross.Txn.Realized.PL
                             
                             
                             market <- market[,list(Instrument, Date, Close)]
                             setnames(market,"Close","Price")
                             start <- min(assets[,.SD[1] ,by=Instrument]$Date) # start from the first available position, not from the first market price
                             marked.portfolio <- assets[market[Date>=start], roll=TRUE][, Value:=Pos * Price]
                             # handle missing TxnValue - fill zeroes alternative
                             cols <- c("Instrument", "Date", "Pos", "Price", "Value")
                             valued <- marked.portfolio[, cols, with=FALSE]
                             with.txns <- valued[txns][, c(cols, "TxnValue"), with=FALSE]
                             no.txns <- valued[!txns][,TxnValue:=0]
                             valued <-  .rbind.data.table(with.txns, no.txns)
                             setkey(valued, Instrument, Date)
                             # handle missing (NA) TxnValue - is.na() alternative
                             #   out <- txns[,list(Instrument,Date,TxnValue)][marked.portfolio]
                             valued[, Prev.Value:=delay(Value, pad=0), by=Instrument]
                             assets <<- valued[, PL:= Value - Prev.Value - TxnValue]
                             return(assets)
                           })
)

##### ACCOUNT #######

Account <- setRefClass("Account"
                       , contains="Portfolio"
                       , fields = list(entries="data.table"
                                       ,portfolios=function(l) {
                                         # list of portfolios
                                         if(missing(l)) return(invisible(assets))
                                         else {
                                           if(length(l)==1) {
                                             assets <<- l[[1]]$assets
                                             txns <<- l[[1]]$txns
                                           } else {
                                             pool.Portfolio <- function(x,y) {}
                                             assets <<- pool.Portfolio(NULL,NULL) # stump
                                           }
                                         }
                                       }
                                       ,benchmarks="data.table")
                       # Deposits + Withdrawals + Realized PL + Unrealized PL + Interest Income
                       , methods = list(
                         
                         initialize=function(...)  {
                           assign('.performance',numeric(), .self)
                           .self$initFields(...)
                         },
                         
                         deposit=function(amount, date) {
                           
                         },
                         
                         withdraw=function(amount, date) {
                           
                         })
)

##### EXPERIMENTAL #####

#' constructor for creating an portfolio object.
assets <- function(symbols){
  
}
#' TODO: vectorize pattern
Search <- function(pattern, category="instrument") {
  View(instrument.table(find.instrument(pattern)))
}

