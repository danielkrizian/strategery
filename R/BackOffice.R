#' @include Portfolio.R
BackOffice <- setRefClass("BackOffice",
                                fields=list(
                                  portfolio="Portfolio", # ?
                                  market="data.table" # ?
                                ),
                                methods=list(
  valueHoldings = function(){
    .market <- market[,list(Instrument, Date, Close)]
    setnames(.market,"Close","Price")
    
    positions = portfolio$positions
    
    # start from the first available position, not from the first market price
    START = positions[,list(First=min(Date)), by=Instrument]
    bounded.market = .market[START][Date>=First][,First:=NULL]
    setkey(bounded.market, Instrument, Date)
    holdings = positions[bounded.market, roll=TRUE][, Value:=Pos * Price]
    # handle missing TxnValue - fill zeroes alternative
    cols = c("Instrument", "Date", "Pos", "Price", "Value")
    portfolio$positions <<- holdings[, cols, with=FALSE]
  },
  
  bookFills = function(x){
    " Update portfolio positions with new transactions.
                                    
                                    x - data.table object with columns:
                                        Instrument, Date, TxnQty, Price, TxnValue
                                        keyed by Instrument, Date"
    
    fills = portfolio$addFills(x)
  }
                                )
)