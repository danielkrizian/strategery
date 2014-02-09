
#' constructor for creating an portfolio object.
#' portfolio object represents implementation of a strategy, and hence have the same name.
portfolio <- function(data) {
  
  if(missing(data))
    return(Portfolio$new())
  
  Portfolio$new(assets=data)
}



update.portfolio <- function(portfolio, txns) {
  .LastPos <- function(portfolio, Instrument) {
    LastPos <- last(portfolio[Instrument==Instrument,])$Pos
    return(LastPos)
  }
  txns[,Pos:=.LastPos(portfolio, Instrument) + cumsum(TxnQty), by=Instrument]
  portfolio <- .rbind.data.table(portfolio
                                 , txns[,list(Instrument, Date, Pos)], use.names=TRUE)
  setkey(portfolio, Instrument, Date)
  
  return(portfolio)
}

# Portfolio <- setRefClass("Portfolio", 
#                          fields = list(holdings = "numeric", 
#                                        value=function(v) {
#                                          sum(holdings)
#                                        })
# )
# 
# Portfolio$new(holdings =c(1055.43, 345.7))
# p$value
# p$value
# Portfolio
# 
# pos <- setRefClass("pos", 
#                          fields = list(last = "numeric")
# )
# portf <- setRefClass("portf", 
#                          fields = list(pos = "pos")
#                      , methods = list(lastpos = function(v=0) {
#                        pos$last
#                      }))
# 
# opos <- pos$new(last=1)
# oportf <- portf$new(pos=opos)
# oportf$lastpos()

portfolio.PL <- function(portfolio, txns, market=OHLCV){
  
  market <- market[,list(Instrument, Date, Close)]
  setnames(market,"Close","Price")

    # marked.portfolio shows NA positions (1928-02-04)
  marked.portfolio <- portfolio[market, roll=TRUE][, Pos.Value:=Pos * Price]
  # handle missing TxnValue - fill zeroes alternative
  cols <- c("Instrument", "Date", "Pos", "Price", "Pos.Value")
  valued <- marked.portfolio[, cols, with=FALSE]
  with.txns <- valued[txns][, c(cols, "TxnValue"), with=FALSE]
  no.txns <- valued[!txns][,TxnValue:=0]
  valued <-  .rbind.data.table(with.txns, no.txns)
  setkey(valued, Instrument, Date)
  
  # handle missing (NA) TxnValue - is.na() alternative
  #   out <- txns[,list(Instrument,Date,TxnValue)][marked.portfolio]
  
  valued[, PL:= Pos.Value - delay(Pos.Value) - TxnValue]
  return(valued)
}


# TODO: temporary as.portfolio uses falcon-specific load.portfolio
as.portfolio <- function(x){
  return(load.portfolio(x))
}

#' Load portfolio data for a pre-defined instrument object
#' 
#' src Should be defined in the GlobalEnv. 
#' Alternatively, define it in each instrument
#' @export
load.portfolio <- function(id
                      , interval=c("days","weeks","months")) {
  
  interval <- match.arg(interval, c("days","weeks","months"))
  if(!attr(src,"connected"))
    src <- connect(src)
  
  if(is.character(id) || is.double(id))
    instr <- try(getInstrument(id), silent=TRUE)
  if(!inherits(instr,"instrument") || !inherits(instr,"portfolio"))
    stop("Portfolio doesn't exist.")
  
  id <- instr$identifiers$Falcon
  
  if(grepl("FalconDB", attr(src,"connection.string"))) {
    historical <- TRUE
    sql <- paste("SET NOCOUNT ON; 
                   DECLARE @instruments AS IntList;
                   INSERT INTO @instruments (Value)
                   VALUES(",
                 id,");
                   EXEC getPerformance_Portfolio 
                   @ids=@instruments, 
                   @drillDown=2,
                   @inclProxies=1,
                   @inclRedeemed=",paste(as.integer(historical)),sep="")
    data <- as.data.table(sqlQuery(src, sql))
    
    #two parts, because database stores data in two unioned tables - Price and Return  
    part.prices <- as.prices(data[is.na(Interval),])
    part.returns <- as.prices(as.returns(data[!is.na(Interval) & ProxyID!=10,]))
    
    prices <- rbindlist(list(part.prices, part.returns))
    
    part.prices <- as.returns(part.prices, interval=interval)
    part.returns <- as.returns(part.returns, interval=interval)
    
    ret <- as.returns(rbindlist(list(part.prices, part.returns)))

  }
  instrument_attr(instr$primary_id, "returns", ret)
  instrument_attr(instr$primary_id, "prices", prices)
  return(list(returns=ret, prices=prices))
}

mtm <- function(portfolio, txns, market=OHLCV) {
  txns[market]
} 

enter <- function (){
  
}

buy <- function(){
  
}