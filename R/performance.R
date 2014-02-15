# principles: assume no NAs
# lowest level statistics should be all based on single asset
# vectorize multi-asset on higher level

# Performance <- setRefClass("Performance"
#                            , fields = list(data="data.table",
#                                            assets="character"
#                            )
#                            , methods = list(
#                              
#                              
#                              
#                            ))

# ' Construct price object
prices <- function(..., symbol=NULL,
                   date.col="Date", asset.col="Instrument", val.col="Price") {
  
  dots <- list(...)
  if(!is.null(symbol))
    for(i in 1:length(dots)){
      setattr(dots[[i]], "symbols", symbol[i])
    }
  
  .prices <- function(data, symbol) {

    symbol <- attr(data, "symbols")
    data <- as.data.table(data)
    
    if(asset.col %in% colnames(data)) {
      if(length(unique(data[,asset.col,with=F]))==1 & !anyDuplicated(data[,date.col,with=F])) {
        # single asset, includes asset.col
        return(data)
      }
    } else {
      # wide multi-asset or single without asset.col
      if(length(symbol)==1) {
        #single asset, doesn't include asset.col
        
        renamePriceCol <- function(x, prefer=NULL) {
          
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
            if (!identical(loc, integer(0))) {
              new <- names(x)
              new[loc] <- "Price"
              setnames(x, old=names(x), new= new)
              return(new)
            }
            else stop("subscript out of bounds, no price was discernible from the data")
          }
        }
        renamePriceCol(data)
        
        data[, Instrument:=symbol]
        return(data)
      } else {
        # multi asset
        id.vars <- match("Date", names(data))
        measure.vars <- colnames(data)[!colnames(data) %in% date.col]
        require(reshape2)
        return( melt(data, id.vars=id.vars, measure.vars=measure.vars,
                     variable.name="Instrument", value.name="Price",
                     na.rm=TRUE, variable.factor=FALSE) )
      }
    }
  }
  
  out <- rbindlist(lapply(dots, .prices))
  
  new <- c("Instrument","Date", "Price")
  if(asset.col!="Instrument" | date.col!="Date" | val.col!="Price")
    setnames(out
             , old=c(asset.col,date.col, val.col)
             , new=new)
  setcolorder(out, c("Instrument", "Date", "Price", setdiff(names(out), new)))
  out[, Date:=as.IDate(Date)]
  setkey(out, Instrument, Date)
  out <- out[,new, with=F]
  setattr(out, "class", c("prices", class(out)))
  return(out)
}

#' Constructor for returns object
#' 
#' Header is Instrument, Date, Return. 
#' returns are in data.table with either single or multi-assets, either tall or wide format
#' @param symbol name for the instrument, if not supplied within ... object. 
#' @param ... Multi-instrument or single instrument data.
#' @param rf character list of  
#' Rf and benchmarks are stored as list(InstrumentName=Rf) or 
#' list(InstrumentName=Rb) pairs in the attributes
#' and their return data aligned by Date and rbinded for later retrieval
returns <- function(..., symbol=NULL, rf=NULL, bench=NULL, as.interval="irregular",
                    date.col="Date", asset.col="Instrument", val.col="Return") {
  # instruments tall = "Instrument" column or single-asset
  # instruments wide = 
  
  dots <- list(...)
  if(is.prices(dots[[1]]) & length(dots)==1)
    return(as.returns.prices(dots[[1]]))
  
  if(!is.null(symbol))
    for(i in 1:length(dots)){
      setattr(dots[[i]], "symbols", symbol[i])
    }
  
  .returns <- function(data, symbol) {
    symbols <- attr(data, "symbols")
    data <- as.data.table(data)
    if(asset.col %in% colnames(data)) {
#       if(length(unique(data[,asset.col,with=F]))==1 & !anyDuplicated(data[,date.col,with=F])) {
        # single asset, includes asset.col
        return(data)
#       } else {
        
#       }
      
    } else {
      # wide multi-asset or single without asset.col
      if(length(symbols)==1) {
        #single asset, doesn't include asset.col
        data[, Instrument:=symbols]
        return(data)
      } else {
        # multi asset
        id.vars <- match("Date", names(data))
        measure.vars <- colnames(data)[!colnames(data) %in% date.col]
        require(reshape2)
        return( melt(data, id.vars=id.vars, measure.vars=measure.vars,
                     variable.name="Instrument", value.name="Return",
                     na.rm=TRUE, variable.factor=FALSE) )
      }
    }
  }
  
  out <- rbindlist(lapply(dots, .returns))
  
  new <- c("Instrument","Date", "Return")
  if(asset.col!="Instrument" | date.col!="Date" | val.col!="Return")
    setnames(out
             , old=c(asset.col,date.col, val.col)
             , new=new)
  setcolorder(out, c("Instrument", "Date", "Return", setdiff(names(out), new)))
  
  if(!identical(as.interval,"irregular"))
    if(identical(as.interval,"monthly"))
      out[,Date:=as.yearmon(Date)]
  
  out[, Date:=as.IDate(Date)]
  setkey(out, Instrument, Date)
  out <- out[,new, with=F]
  class(out) <- c("returns", class(out))
  
  if(is.returns(bench)) {
    
    setkey(out, Date)
    setkey(bench, Date)
    out <- bench[out]
    setnames(out,
             c("Date" ,"Instrument",  "Return" ,"Instrument.1", "Return.1"),
             c("Date" ,"BM.Instrument",  "Benchmark" ,"Instrument", "Return"))
    out <- out[,list(Instrument, Date, Return, Benchmark, BM.Instrument)]
    setkey(out, Instrument, Date)
    
  }
  
  #   l <- as.data.table(list(A="b",C=c("d","e")))
  #   fun <-  function(i) data.table(Instrument=names(l)[[i]],
  #                                  Number=seq_along(l),
  #                                  Benchmark=l[[i]])
  #   setattr(out, "bmmap" , rbindlist(lapply(seq_along(l), fun )))
  
  out
}

  #' Coerce to prices class - time series of prices
  #' 
  #' @export
as.prices <- function(x, ...) {
  UseMethod("as.prices",x)
}

#' Coerce to returns class - time series of returns
#' 
#' @export
as.returns <- function(x, ...) {
  UseMethod("as.returns",x)
}

as.returns.prices <- function(x, ...) {
  
  x[, Return:=Price / c(NA,head(Price,-1)) - 1 , by=Instrument]
  x[, Price:=NULL]
  x <- x[!is.na(Return)]
  setattr(x, "class", c("returns", setdiff(class(x), "prices")))
  x
}

# as.prices.default <- function(x, interval) {
#   
#   columnsInput <- c("Name", "Date", "Value")
#   columnsOutput <- c("Name", "Date", "Value")
#   .key <- c("Name","Date")
#   
#   ans <- as.data.table(x)
#   
#   ans[, Date := as.IDate(Date)]
#   ans <- ans[, columnsInput, with=FALSE ]  # setcolorder(ans, newNames) probably not needed
#   setnames(ans, columnsOutput)
#   
#   if(!missing(interval))
#     ans <- to.interval(ans, to=interval)
#   
#   setkeyv(ans, .key)
#   setattr(ans, "class", c(attr(ans,"class"), "prices"))
#   #setattr(ans, "ohlc", ohlc)
#   return(ans)
# }


# 
# as.returns.prices <- function(x, interval=c("days","weeks","months","years")) {
#   
#   columnsInput <- c("Name", "Date", "Value")
#   columnsOutput <- c("Name", "Date", "Value")
#   .key <- c("Name","Date")
#   
#   ans <- x[, columnsInput, with=FALSE ]  # setcolorder(ans, newNames) probably not needed
#   setnames(ans, columnsOutput)
#   
#   if(missing(interval)) {
#     #compress to the lowest common interval
#     lci <- max(prices[,list("Interval"=periodicity(Date)$frequency),by=Name]$Interval)
#     interval <- as.units.frequency(lci)
#   }
#   
#   ans <- to.interval(ans, to=interval)
#   ans[!is.na(Value),Value:=roc(Value, na.pad=T), by=Name]
#   
#   # returns should be first, but problems with RStudio data viewer
#   setattr(ans, "class", c(attr(ans,"class"), "returns")) 
#   setattr(ans, "interval", interval)
#   return(ans)
# }


is.returns <- function(x) {
  inherits(x, "returns")
}

is.prices <- function(x) {
  inherits(x, "prices")
}

monthly <- function(x, ...) {
  UseMethod("monthly",x)
}

monthly.prices <- function(x) {
  x <- x[endpoints(Date, on="months")][,Date:=as.IDate(as.yearmon(Date))]
  setattr(x, "class", c("prices", class(x)))
  return(x)
}

monthly.returns <- function(x){
  
}


#' Calculate summary statistics for returns
#' 
#' @param byIns a logical value. Calculate for each Instrument separately?
#' @param byPer Either logical (TRUE is equivalent to "years") or character, one of 
#' c("months","years")
#' @param weights a numeric vector or data.table of portfolio weights. If provided,
#' portfolio returns are calculated.
summary.returns <- function(x, byIns=ifelse(is.null(weights),T,F), byPer=F, weights=NULL) {
  

  if(!is.na(match(byPer, c("months", "years", T))))
     return(summary.returns.by.period(x=x, byIns=byIns, weights=weights))
     
  ann=252
  compound=T

  by= if(byIns) "Instrument" else NULL
  
  
  x[, Equity:= cumprod(1+Return), by=Instrument]
  x[, Drawdown:= Equity / cummax(Equity) - 1, by=Instrument]
  drawdowns <- x[, summary.drawdowns(Drawdown, Date), by=Instrument]
  
  return(x[,list(
    "CAGR"=annualized(Return, ann=ann, compound=compound)
    , "Total Return"=cumulative(Return, compound=compound)
    , "Sharpe"=sharpe(Return, Rf=0, ann=ann)
    , "Volatility"=sigma(Return, ann=ann)
    , "R2"=r2(cumprod(1+Return))
    , "DVR"= dvr(Return, Rf=0, ann=ann) # dvr(Return)
    , "MAR" = mar(Return, ann=ann)
    , "Max Drawdown"= maxdd(Return)
    , "Average Drawdown"= mean(drawdowns$Depth) #avgdd(Return)
    , "Average Drawdown Length" = mean(drawdowns$Length)
                 )
           , by=by])
}

summary.returns.by.period <- function(x, byIns=ifelse(is.null(weights),T,F), weights=NULL) {
  
}

plot.returns <- function(x) {
  require(ggplot2);  require(ggthemes); require(grid); require(scales)
  x <- x[,list(Instrument,Date,Equity, Drawdown)]
  x <- data.table:::melt.data.table(x, id.vars=c("Instrument","Date"))
  
  p <- ggplot(x, aes(x=Date, y=value, colour=Instrument)) + geom_line() +
    #   scale_y_continuous(trans=log10_trans())
    # p + 
    facet_grid(variable ~ ., scales="free_y") +
    scale_x_date(breaks=pretty_breaks(n=10), minor_breaks="year", labels=date_format("%Y")) + # scale_x_date(breaks=number_ticks(7), labels=date_format("%Y"))
    scale_y_continuous(labels = percent_format()) +
    coord_trans(y="log1p") + 
    #
    theme_economist_white(gray_bg=FALSE) + 
    scale_colour_economist() +
    xlab("") + ylab("Cumulative Performance (Log scale)")
  
  g = ggplotGrob(p)
  panels = which(sapply(g[["heights"]], "attr", "unit") == "null")
  g[["heights"]][panels] = list(unit(12, "cm"), unit(3, "cm"))
  dev.off()
  grid.draw(g)
}

summary.drawdowns <- function(drawdowns, dates=NULL) {
  
  prevdd <- c(0, drawdowns[-length(drawdowns)])
  
  ddstarts = which( drawdowns != 0 & prevdd == 0 )
  ddends = which( drawdowns == 0 & prevdd != 0 )
  if(!length(ddstarts))
    return(data.table(From=as.Date(character(0))
                      ,Trough=as.Date(character(0))
                      , To=as.Date(character(0))
                      , Depth=numeric(0)
                      ,  Length= numeric(0)
                      ,"To Trough"=numeric(0)
                      ,"Recovery"=numeric(0)
                      , key="Depth"))
  
  if(tail(ddends,1)!=length(drawdowns))
    ddends <- c(ddends, length(drawdowns)) # close last incomplete drawdown
  
  ddthroughs <- rbindlist(sapply(1:length(ddstarts), function(x) {
    ddsubset <- drawdowns[ddstarts[x]:ddends[x]]
    depth <- min(ddsubset)
    list(depth=depth,
         index=which(drawdowns==depth)[1])  # take first if multiple matches
  }, simplify=FALSE))
  
  out <- data.table(From=dates[ddstarts]
                    ,Trough=dates[ddthroughs$index]
                    , To=dates[ddends]
                    , Depth=ddthroughs$depth
                    ,  Length= ddends - (ddstarts - 1)
                    ,"To Trough"=ddthroughs$index - (ddstarts - 1)
                    ,"Recovery"=ddends - ddthroughs$index
                    , key="Depth")
  out[order(Depth)]
}

summary.trades <- function(trades) {
  
}

