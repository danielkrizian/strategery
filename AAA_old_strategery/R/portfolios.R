
root.portfolio <- function(date, instruments, data.frame=NULL) {
  if(is.null(data.frame))
  return(data.table(Date=date, Instrument=instruments, key=c("Instrument","Date")))
  else
    data <- as.data.table(data.frame)
    portfolio <- root.portfolio(date=min(data$Date) - 1
                                , instruments=unique(data$Instrument))
  return(portfolio)
}


#' Some Title
#' 
#' @export
portfolio <- function(name="default",
                      symbols=colnames(positions),
#                       initPosQty = 0,
#                       initDate = "1950-01-01",
                      currency="USD",
                      positions=NULL,
                      trades=F,
                      store=T,
                      ...){
#   if(is.null(symbols))
#     stop("Supply symbols to portfolio.")
  P <- list()
  P$name=name
  if(is.null(positions))
    P$pos <-xts(matrix(numeric(0),ncol=ifelse(is.null(symbols),1,length(symbols))), 
                order.by= as.Date.numeric(numeric(0),origin="1970-01-01"))
                 
  else {
    P$pos <- positions
  }
  if(trades)
    P$trades <-Trades(P)
  
  class(P) <- c("portfolio")
  if (store) 
    assign(paste("portfolio", P$name , sep = "."),
           P, envir = .blotter)
  else return(P)
}

#' Some Title
#' 
#' @export
updatePfolio <- function(portfolio,
                         positions=NULL,
                         trades=F,
                         store=T)
{
  if(is.character(portfolio))
    portfolio <- getPortfolio(portfolio)
  if(length(portfolio$pos))
    if(first(index(positions))<=last(index(portfolio$pos)))
      stop("Cannot update portfolio. Positions overlap.")
  
  old.symbols <- colnames(portfolio$pos)
  update.symbols <- colnames(positions)
  ### TODO: symbol additions/removals treatment 
  same.symbols <- intersect(old.symbols, update.symbols)
  new.symbols <- setdiff(update.symbols, old.symbols)
  
  if(is.null(old.symbols))
    portfolio$pos <- positions
  else {
   portfolio$pos <- rbind.xts(portfolio$pos[,same.symbols], 
                              positions[,same.symbols])
   portfolio$pos <- cbind.xts(portfolio$pos,
                              positions[,new.symbols],all=T, fill=0)
  }
  if(trades)
    portfolio$trades <- Trades(portfolio)
  if (store) 
    assign(paste("portfolio", portfolio$name, sep = "."),
           portfolio, envir = .blotter)
  else return(portfolio)
}

#' Some Title
#' 
#' @export
ValuationPrices <- function(x) {
  # for each signal/position, get latest price
  prices <- Align(market$prices,to=x,pad=F)
  if(market$has.NAs)
    prices <- na.locf(prices,na.rm=F)
  return(prices)
}

#' Some Title
#' 
#' @export
Returns <- function(portfolio, #portfolio
                    ..., # other portfolios to merge returns with
                    type=c("periods","trades")[1],
                    reduce=F, #remove all-zero rows?
                    refresh=T) {
  # TODO: storage functionality
      # TODO: consider Portfolio Valuation operation in general
  # TODO: method for portfolio
  other <- list(...)

  if(!length(other)){
    if(is.character(portfolio))
      portfolio <- getPortfolio(portfolio)
    if(!refresh & type!="trades" & !is.null(portfolio$R))
      return(portfolio$R)
    if(type=="trades")
      return(Trades(portfolio,refresh=refresh))

    out <- portfolio$pos * market$returns
    
    if(NCOL(out)>1)
      out[] <- rowSums(out, na.rm=T)
    if(reduce)
      out <- out[out!=0]
    return(out)
  } else {
    portfolio <- c(list(portfolio), other)
    if(type!="trades") combine=cbind else combine=c
    require(foreach)
    foreach(portfolio=iter(portfolio), .combine=combine) %do% {
      if(is.character(portfolio))
        portfolio <- getPortfolio(portfolio)
      if(!refresh & type[1]!="trades" & !is.null(portfolio$R))
        return(portfolio$R)
      if(type[1]=="trades"){
        out <- list(Trades(portfolio,refresh=refresh)$'return')
        names(out) <- portfolio$name
        return(out)
      }  
      out <- portfolio$pos * market$returns
      if(NCOL(out)>1)
        out[] <- rowSums(out, na.rm=T)
      if(reduce)
        out <- out[out!=0]
      return(out)
    }
  }
}

#' Some Title
#' 
#' @export
Trades <- function (portfolio, refresh=T) {
      # TODO: support for "share" type sizing
  if(is.character(portfolio))
    portfolio <- getPortfolio(portfolio)
  
  if(!refresh & !is.null(portfolio$trades))
    return(portfolio$trades)
  
  pos <- portfolio$pos
  symbols <- colnames(pos)
  if(length(colnames(pos)) < NCOL(pos)) {
    symbols <- colnames(pos) <- colnames(market$prices)  # QUICK FIX
    warning("Symbol names not found in position matrix. Filled from market$prices.")
  }

  if(any(pos[1,]!=0)){ # include 10 days prior
    fd <- index(pos[1,])
    pd <- index(last(market$prices[paste(fd-10, fd-1, sep="::")]))
    pos.add <- xts(0, order.by=pd )
    pos <- rbind(pos.add,pos)
  }
    
  pos.next <- lag.xts(pos, -1, na.pad=T)
  trade <- lag.xts(diff.xts(pos),-1,na.pad=T)

  tstart <- trade & pos.next != 0
#   tend <- trade & pos.next == 0 # didnt work for long-short strategy
  tend <- trade !=0 & pos !=0
  
  if(sum(tstart, na.rm=T) - sum(tend, na.rm=T) > (NCOL(pos)))
    warning("Trade start and end imbalance. Check Trades function.")
    
  prices <- ValuationPrices(pos)
  trades <- c()
    
  for( sym in 1:NCOL(pos)) {
    
    tstarti = which(tstart[,sym])
    tendi = which(tend[,sym])
    ntrades <- length(tstarti)
    if( ntrades > 0 ) {
      if( length(tendi) < ntrades )
        tendi <-  c(tendi, NROW(pos))
      trades <- 
        rbind(trades,
              data.frame(symbol=      rep(symbols[sym],ntrades),
                         size=        as.vector(pos[tendi, sym]),
                         entry.date=  index(pos[tstarti,]),
                         exit.date=   index(pos[tendi,]),
                         entry.price= as.vector(prices[tstarti, sym]),
                         exit.price=  as.vector(prices[tendi, sym])))
    }
  }
  if(length(trades)) {
    trades$return = 
      round( 100* trades$size * (trades$exit.price/trades$entry.price - 1),2)
    trades$entry.price= round(trades$entry.price,2)
    trades$exit.price=  round(trades$exit.price,2)
  }
  return(trades)
}

#' Some Title
#' 
#' @export
Equity <- function
(... # portfolios
 ,R=Returns(...)
 ,nas=any(is.na(R)) # logical, does R contain NAs? If NULL, function will detect, but costly speedwise
 ,init=100 # numeric, value of the starting index
 ,lead.na.rm=F # Should leading NAs be removed?
 ,dates=c("first","longest") #if "first", R will be subset by indices of first series
 ,short.align=c("first", "longest", "none") # align start of the shorter indices to which series?
 ,drawdown=F # return both index and drawdown in list? 
 ,names=NULL
) {
#   TODO: lead.na.rm=T not tested

  if(length(names))
    colnames(R) <- names[1:NCOL(R)]
  
  #### quick script for clean version of R
  if(!nas) {
    idx <- cumprod(1+R)
    if(!drawdown) return(idx)
  }
  
  #### helper functions
  .equity <- function(x) cumprod(1+x)
  .peak <- function(x) cummax(x)
  applyNAsafe <- function(x, .fun, ...){
    cleanx <- na.omit(x)
    if(length(attr(cleanx,"na.action"))){
      out <- x
      out[attr(cleanx,"na.action")] <- NA
      out[-attr(cleanx,"na.action")] <- do.call(.fun, list(cleanx, ...) )
    } else
      out <- do.call(.fun, list(cleanx, ...) )
    return(out)
  }
  
  #### preprocess data
  # has leading NAs or just middle NAs
  # middle NAs - fill them with zeroes; leading NAs - leave them in place
  dates <- dates[1]
  if (dates == "first") {
    first.dates <- string.range(na.omit(R[,1]))
    R <- R[first.dates]
  }
       
  .data <- coredata(R)
  lead.na <- if(lead.na.rm) NULL else NA
  .data <- apply(.data, 2 , na.fill, fill=list(lead.na,0,0))

  #### calculate equity index
  idx <- apply(.data,2, applyNAsafe , .fun=.equity)
  
  #### align shorter indices
  short.align <- short.align[1]
  if(short.align=="none")
    init <- rep(init,NCOL(R))
  else if(short.align=="longest"){
    i.lastna <- colSums(is.na(idx))
    longest <- which.min(i.lastna)
    init <- sapply(i.lastna, function(x) if(x>0) init * idx[x, longest] else init)
  } else if(short.align=="first") {
    i.lastna <- colSums(is.na(idx))
    init <- sapply(i.lastna, function(x) if(x>0) init * idx[x, 1] else init)
  }
  idx <- t(t(idx) * init) # multiply matrix by vector

  #### calculate drawdown
  if(!drawdown)
    return(reclass(idx,R))
  else {
    out <- list()
    peak <- apply(idx, 2, applyNAsafe , .fun=.peak)
    dd <- idx/peak - 1
    return(list(index=reclass(idx,R),drawdown=reclass(dd,R)))
  }
}



# $symbols
# $symbols$SPX
# $symbols$SPX$txn
#            Txn.Qty Txn.Price Txn.Value Txn.Avg.Cost Pos.Qty
# 1950-01-01       0         0         0            0       0
#            Pos.Avg.Cost Gross.Txn.Realized.PL Txn.Fees
# 1950-01-01            0                     0        0
#            Net.Txn.Realized.PL Con.Mult
# 1950-01-01                   0        0
# 
# $symbols$SPX$posPL
#            Pos.Qty Con.Mult Ccy.Mult Pos.Value Pos.Avg.Cost
# 1950-01-01       0        1        1         0            0
#            Txn.Value Period.Realized.PL Period.Unrealized.PL
# 1950-01-01         0                  0                    0
#            Gross.Trading.PL Txn.Fees Net.Trading.PL
# 1950-01-01                0        0              0
# 
# $symbols$SPX$posPL.USD
#            Pos.Qty Con.Mult Ccy.Mult Pos.Value Pos.Avg.Cost
# 1950-01-01       0        1        1         0            0
#            Txn.Value Period.Realized.PL Period.Unrealized.PL
# 1950-01-01         0                  0                    0
#            Gross.Trading.PL Txn.Fees Net.Trading.PL
# 1950-01-01                0        0              0
# 
# 
# 
# $summary
#            Long.Value Short.Value Net.Value Gross.Value Realized.PL
# 1950-01-01          0           0         0           0           0
#            Unrealized.PL Gross.Trading.PL Txn.Fees Net.Trading.PL
# 1950-01-01             0                0        0              0
# 
