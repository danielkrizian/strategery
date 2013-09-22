
newStrategy <- function(name="default") {
  
  }

runLength <- function(x) {
  (x) * unlist(lapply(rle(as.vector(x))$lengths, seq_len))
}


#' Number of barse since a condition has been met
#' 
#' @export
BarsSince <- function(x) runLength(!x)

#' Remove excessive signals
#' 
#' returns 1 on the first occurence of "true" signal in x
#' then returns 0 until y is true even if there are "true" signals in x
#' @export
ExRem <- function(x,y=!x) {
  filter=FALSE
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  
  for (i in 1:length(x)) {
    if(filter) {
      if(x[i]) x[i] <- FALSE
      if(y[i]) filter <- FALSE
    }
    if(x[i]) filter <- TRUE
  }
  x
}

#' Remove excessive signals
#' 
#' works as a flip/flop device or "latch" (electronic/electric engineers will know what I mean
#' returns 1 from the first occurence of TRUE signal in x
#' until a TRUE occurs in y which resets the state back to zero
#' unil next TRUE is detected in x...  
#' this essentially reverts the process of ExRem - multiple signals are back again
#' TEST : fill(c(1,1,0,1),c(1,0,0,0))
#' @export
Fill <- function(x,y=!x) {
  x[is.na(x)] <- FALSE
  y[is.na(y)] <- FALSE
  latch <- FALSE
  for (i in 1:length(x)) {
    if(x[i]) latch <- TRUE
    if(y[i]) latch <- FALSE
    if(latch) x[i] <- TRUE
    #     if(y[i]) latch <- FALSE # include also this line in a variant where x=T where y=T
  }
  x
}

#' Remove excessive signals
#' Gives a "1" or true on the day that x crosses above y Otherwise the result is "0".
#' To find out when x crosses below y, use the formula Cross(y, x) 
#' @export
Cross <- function(x, y) {
  above <- x > y
  #below <- y < x
  ExRem(above)
}


lag2 <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  if(k>0) {
    return( c(rep(NA, k), x)[1 : length(x)] )
  }
  else if(k<0) {
    k
    return( c(x[(-k+1):length(x)], rep(NA, -k)) )
  }
  else if(k==0)
    return(x)
    
}

#' blabla
#' 
#' @export
Backtest <- function() {
  buydelay <- getOption("TradeDelays")$Buy
  selldelay <- getOption("TradeDelays")$Sell
  shortdelay <- getOption("TradeDelays")$Short
  coverdelay <- getOption("TradeDelays")$Cover
  if(!exists("Buy")) Buy <- quote(FALSE)
  if(!exists("Sell")) Sell <- quote(FALSE)
  if(!exists("Short")) Short <- quote(FALSE)
  if(!exists("Cover")) Cover <- quote(FALSE)
  BuyPrice <- getOption("BuyPrice")
  SellPrice <- getOption("SellPrice")
  ShortPrice <- getOption("ShortPrice")
  CoverPrice <- getOption("CoverPrice")
  
  if(is.null(BuyPrice) | is.null(SellPrice)| is.null(ShortPrice) | is.null(CoverPrice))
    stop("Undefined Buy/Sell/Short/Cover Prices.")
  
  AddColumn(Buy)
  AddColumn(Sell)
  AddColumn(Short)
  AddColumn(Cover)
  AddColumn(BuyPrice)
  AddColumn(SellPrice)
  AddColumn(ShortPrice)
  AddColumn(CoverPrice)
  
  #R[,el:=eval(Buy,envir=.SD), by=Instrument]
  #R[,xl:=eval(Sell,envir=.SD), by=Instrument]
  #R[,es:=eval(Short,envir=.SD), by=Instrument]
  #R[,xs:=eval(Cover,envir=.SD), by=Instrument]
  
  R[,Sell:=ExRem(Sell, Buy), by=Instrument]
  R[,Buy:=ExRem(Buy,(Sell|Short)), by=Instrument]
  R[,Cover:=ExRem(Cover,Short), by=Instrument]
  R[,Short:=ExRem(Short,(Cover|Buy)), by=Instrument]
  
  R[,Buy:=lag2(Buy, getOption("TradeDelays")$Buy), by=Instrument]
  R[,Sell:=lag2(Sell, getOption("TradeDelays")$Sell), by=Instrument]
  R[,Short:=lag2(Short ,getOption("TradeDelays")$Short), by=Instrument]
  R[,Cover:=lag2(Cover, getOption("TradeDelays")$Cover), by=Instrument]
  
  R[,Pos:=Fill(Buy, Sell|Short)-Fill(Short, Cover|Buy), by=Instrument]
  
  PrevClose <- lag2(R$Close, 1)
  R[,Return:=ifelse(Buy, Close/BuyPrice - 1
                    , ifelse(Sell, SellPrice/PrevClose - 1
                             , ifelse(Short, Close/ShortPrice - 1,
                                      ifelse(Cover, CoverPrice/PrevClose - 1
                                             ,Pos * Raw )))), by=Instrument]
  return(R)
}

#' blabla
#' 
#' @export
AddColumn <- function(x, name) {
  if(missing(name))
    name <- deparse(substitute(x))
  if(is.call(x) | is.symbol(x) | is.vector(x)) {
    R[,eval(substitute(name)):=eval(x,envir=.SD), by=Instrument]
  } else if(is.data.table(x)) {
    R <- x[R, roll=TRUE, nomatch=NA]
  }
  #assign("R",value=out,inherits=TRUE)
  
}
