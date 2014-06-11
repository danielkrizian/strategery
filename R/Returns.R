#' Construct returns object
#' 
#' @rdname indicator
#' @export indicator
returns <- function(data, trans, col, benchmark=NULL) {
  if(missing(trans))
    strans <- as.name(col)
  else
    strans <- substitute(trans)
  r = Returns$new(data=data, trans=strans, benchmark=as.character(benchmark))
  return(r$eval())
}

Returns.calcAlpha <- function(annualize=T) {
  #TODO(dk): finalize Returns.alpha. Signature: benchmark data.table, Rf data.table
  Rf=0
  data[, list(Alpha=alpha(Return, Benchmark, Rf)), by=Instrument]
}

Returns.calendar <- function(what=c("MTD", "YTD", "3M", "6M", "years")){
  freq <<- 12L; warning("Freq fixed at 12 in Returns.calendar. TODO")
  years <- data[, list(Return=prod(1+Return)-1), by=list(Instrument, Year=year(Date))]
  years <- dcast.data.table(years, Instrument ~ Year)
  all <- data[, list("Total (ann.)"=prod(1+Return)^(freq/.N)-1),keyby="Instrument"]
  out <- merge(all, years)
  return(out)
}

Returns.correlation <- function(with="Benchmark"){
  if(identical(with,"Benchmark"))
    data[, list(Correlation=cor(Return,Benchmark)),keyby=Instrument]
}

#' @import ggplot2
#' @import scales
#' @import ggthemes
#' @import grid
Returns.plot <- function(drawdowns=T) {
  datacols=c(.id, .time, .self$index)
  if(drawdowns)
    datacols = c(datacols, .self$drawdowns)
  x = data[, datacols, with=FALSE]
  x = data.table:::melt.data.table(x, id.vars=c("Instrument","Date"))
  
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

Returns.summary <- function(weights=NULL) {
    
  ann=252
  compound=T
  Return = as.name(.col)
  
  by= if(is.null(weights)) "Instrument" else NULL
  
  data[, Equity:= cumprod(1+eval(Return)), by=Instrument]
  data[, Drawdown:= Equity / cummax(Equity) - 1, by=Instrument]
  dd <- data[, summary.drawdowns(Drawdown, Date), by=Instrument]
  
  return(data[,list(
    "CAGR"=annualized(eval(Return), ann=ann, compound=compound)
    , "Total Return"=cumulative(eval(Return), compound=compound)
    , "Sharpe"=sharpe(eval(Return), Rf=0, ann=ann)
    , "Volatility"=sigma(eval(Return), ann=ann)
    , "R2"=r2(cumprod(1+eval(Return)))
    , "DVR"= dvr(eval(Return), Rf=0, ann=ann) # dvr(Return)
    , "MAR" = mar(eval(Return), ann=ann)
    , "Max Drawdown"= maxdd(eval(Return))
    , "Average Drawdown"= mean(dd$Depth) #avgdd(Return)
    , "Average Drawdown Length" = mean(dd$Length)
  )
  , by=by])
}

#' @include Indicator.R
Returns = setRefClass('Returns', contains="Indicator",
                      fields= list(.dd = "character",
                                   .vindex = "character",
                                   benchmark="character",
                                   drawdowns=function(col){
                                     if(missing(col)){
                                       if(!length(.dd)){
                                         if(length(.vindex)) {
                                           Index = as.name(.vindex)
                                           data[, Drawdown:=Index / cummax(Index) - 1
                                                , by=.id]
                                         } else {
                                           Return = as.name(.col)
                                           data[,Drawdown:=dd(eval(Return)), by=.id]
                                         }
                                         .dd <<- "Drawdown"
                                       }
                                       return(.dd)
                                     } else {
                                       .dd <<- col
                                     }
                                   },
                                   index=function(col){
                                     if(missing(col)){
                                       if(!length(.vindex)){
                                         Return = as.name(.col)
                                         data[,Index:=value.index(eval(Return)), by=.id]
                                         .vindex <<- "Index"
                                       }
                                       return(.vindex)
                                     } else {
                                       .vindex <<- col
                                     }
                                   }),
                      methods = list(calcAlpha=Returns.calcAlpha,
                                     calendar=Returns.calendar,
                                     correlation=Returns.correlation,
                                     plot=Returns.plot,
                                     summary=Returns.summary))




###### To Delete ####################################

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
