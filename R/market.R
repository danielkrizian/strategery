updateSymbols <- function(Symbol,src="yahoo") {
  .env <- environment()
  getSymbols(Symbol, src="FI"
             ,dir="Data/Market"
             ,from="1900-01-01"
             ,split_method="common"
             ,use_identifier="primary_id"
             ,env=.env)
  old <- get(Symbol,pos=.env)
  from <- index(last(old)) + 1
  sl <- list()
  sl[[Symbol]] <- list(src=src, name=getInstrument(Symbol)$identifiers[[src]])
  setSymbolLookup(sl)
#   last.update <- attr(get(Symbol,pos=.env),"updated")
  getSymbols(Symbol, adjust=FALSE, from=from, env=.env)
    append <- get(Symbol, pos=.env)
  if(NROW(append)) {
    assign( Symbol, rbind(old, append ), pos=.env)
    saveSymbols.common(Symbol, base_dir="Data/Market", env=.env)
  }
}

getPrice <- function (x, symbol = NULL, prefer = NULL) 
{
  if(is.environment(x)) {
    x <- x[[symbol]]
    symbol <- NULL
  }

  if (!is.null(symbol)) {
    loc <- grep(symbol, colnames(x))
    if (!identical(loc, integer(0))) {
      x <- x[, loc]
    }
    else {
      stop(paste("subscript out of bounds: no column name containing", 
                 symbol))
    }
  }
  if (is.null(prefer)) {
    if (has.Price(x)) 
      prefer = "price"
    else if (has.Trade(x)) 
      prefer = "trade"
    else if (has.Cl(x)) 
      prefer = "close"
    else stop("subscript out of bounds, no price was discernible from the data")
  }
  if (!is.null(prefer)) {
    loc <- NULL
    switch(prefer, Op = , open = , Open = {
      loc <- has.Op(x, which = TRUE)
    }, Hi = , high = , High = {
      loc <- has.Hi(x, which = TRUE)
    }, Lo = , low = , Low = {
      loc <- has.Lo(x, which = TRUE)
    }, Cl = , close = , Close = {
      loc <- has.Cl(x, which = TRUE)
    }, Bid = , bid = {
      loc <- has.Bid(x, which = TRUE)
    }, Ask = , ask = , Offer = , offer = {
      loc <- has.Ask(x, which = TRUE)
    }, Mid = , mid = , Midpoint = , midpoint = {
      loc <- has.Mid(x, which = TRUE)
    }, Trade = , trade = {
      loc <- has.Trade(x, which = TRUE)
    }, Price = , price = {
      loc <- has.Price(x, which = TRUE)
    }, {
      loc <- grep(prefer, colnames(x))
    })
    if (!identical(loc, integer(0))) 
      return(x[, loc])
    else stop("subscript out of bounds, no price was discernible from the data")
  }
  
}

Prepare.market <- function
(
  symbols,  
  #   market, # environment
  align = c('keep.all', 'remove.na'),
  dates = NULL,
  fill.gaps = F
)
{
  # goal: market$prices must contain trading opportunities of the relevant model. Columns are symbols. Make OpenClose frequency if necessary.
  # remove NAs for speed. Retain NAs for accuracy. Info stored in has.NAs
  # TODO: support OpenClose frequency
  
  market <-new.env()
  loadSymbolLookup("SymbolLookup.RData", dir="Data/Market")
  getSymbols(symbols, env=market)
  
  if( !exists('symbolnames', market, inherits = F) ) market$symbolnames = ls(market)
  symbolnames = market$symbolnames
  nsymbols = length(symbolnames)
  if( nsymbols > 1 ) {
    out = bt.merge(market, align, dates)
    for( i in 1:nsymbols ) {
      symbol.data <- market[[ symbolnames[i] ]]
      symbol.data =
        make.xts( coredata( symbol.data )[ out$date.map[,i],, drop = FALSE], out$all.dates)
      map.col = find.names('Close,Volume', colnames(symbol.data))
      if(fill.gaps & !is.na(map.col$Close)) {
        close = coredata(symbol.data[,map.col$Close])
        n = length(close)
        last.n = max(which(!is.na(close)))
        close = ifna.prev(close)
        if(last.n + 5 < n) close[last.n : n] = NA
        symbol.data[, map.col$Close] = close
        index = !is.na(close)
        if(!is.na(map.col$Volume)) {
          index1 = is.na(symbol.data[, map.col$Volume]) & index
          symbol.data[index1, map.col$Volume] = 0
        }
        for(j in colnames(symbol.data)) {
          index1 = is.na(symbol.data[,j]) & index
          symbol.data[index1, j] = close[index1]
        }
      }
      market[[ symbolnames[i] ]] <- symbol.data
    }
  } else {
    if(!is.null(dates)) market[[ symbolnames[1] ]] = market[[ symbolnames[1] ]][dates,]
    out = list(all.dates = index(market[[ symbolnames[1] ]]) )
  }
  market$dates = out$all.dates
  dummy.mat = matrix(double(), length(out$all.dates), nsymbols)
  colnames(dummy.mat) = symbolnames
  dummy.mat = xts(dummy.mat, order.by=out$all.dates)
  for( i in 1:nsymbols ) {
    if( has.Cl( market[[ symbolnames[i] ]] ) ) {
      dummy.mat[,i] = Cl( market[[ symbolnames[i] ]] );
    }
  }
  market$prices = dummy.mat
  market$returns <- ROC(market$prices, type="discrete", na.pad=F)
  market$has.NAs <- any(is.na(market$prices))
    
  signal <<- market$prices[,1]
  signal[] <<- 0
  long <<- signal
  short <<- signal
  
  return(market)
}
