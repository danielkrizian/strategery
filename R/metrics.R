
cagr <- function(value, n, base=100, ann=252) {
  (value/base)^(ann / n) - 1
}

twr <- function(value, base=100) {
  value/base
}

cumulative <- function(x, compound=T) {
  if(compound)
    prod(1+x) - 1
  else
    sum(x)
}

annualized <- function(x, ann=12, compound=F) {
  base = ifelse(compound, 1, 0)
  scaleFUN = if(compound) `^` else `*`
  ann.factor = ann/length(x)
  scaleFUN ( cumulative(x, compound) + base , ann.factor) - base
}

sigma <- volatility <- vol <- std <- stdev <- StDev <- function(x, ann=12) {
  sqrt(ann) * sd(x)
}

##### RISK-ADJUSTED PERFORMANCE #####

sharpe <- function(R, Rf=0,ann=252) {
  excess = R - Rf
  sqrt(ann) * mean(excess) / sd(excess)
}

alpha <- function(R, Rb, Rf=0) {
  merged = as.data.frame(na.omit(cbind(R-Rf, Rb-Rf)))
  model.lm = lm(merged[, 1] ~ merged[, 2], merged)
  coef(model.lm)[[1]]
}

#' Some Title
#' 
#' @param equity
#' @export
r2 <- function(equity) {
  cor(equity, 1:length(equity))^2
}

#' David Varadi's Ratio
#' 
#' @export
dvr <- function(R, Rf=0, ann=252) {
  sharpe(R, Rf=Rf, ann=ann) * r2(cumprod(1+R))
}

# http://www.investopedia.com/terms/m/mar-ratio.asp
mar <- function(R, ann=252) {
  e =  cumprod(1+R)
  n =length(e)
  dd = abs(e / cummax(e) - 1)
  cagr(e[n], n, base=1, ann) / max(dd)
}

####### VALUE INDEX ######

#' @param x vector of  percent changes
value.index <- function(x){
  x = na.fill(x, fill=0)
  cumprod(1+x)
}

####### DRAWDOWNS ######

# Calculate drawdowns
dd <- function(x) {
  e <- value.index(x)
  e / cummax(e) - 1
}

maxdd <- function(x) {
  dd <- dd(x)
  max(abs(dd))
}

# Average drawdown length
avgdd <- function(x) {
  dd <- dd(x)
  prevdd <- c(0, dd[-length(dd)])

  ddstarts = which( dd != 0 & prevdd == 0 )
  ddends = which( dd == 0 & prevdd != 0 )
  
  if(tail(ddends,1)!=length(dd))
    ddends <- c(ddends, length(dd)) # close last incomplete drawdown
    
  if(length(ddends) != length(ddstarts)) {
    cat(dd)
    stop(paste("Error calculating average drawdown. There are", length(ddstarts), "DD starts and ", length(ddends), "DD ends"))
  }

  abs(mean(apply( cbind(ddstarts, ddends), 1, function(x){ min( dd[ x[1]:x[2] ]) } )))
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
  
  if(tail(ddends,1)!=length(drawdowns) & drawdowns[length(drawdowns)] !=0)
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

######## TRADES #######

avgwin <- function(x, extreme=F) {
  wins <- x[x>0]
  if(!extreme)
    wins <- wins[which(wins<max(wins))]
  mean(wins)
}

avgloss <- function(x, scratch=F) {
  mean(x[x<0])
}

winrate <- function(x) {
  sum(x>0)/length(x)
}

winloss <- function(x, extreme=F) {
 avgwin(x, extreme=extreme)/abs(avgloss(x))
}

expectancy <- function(x) {
#   http://www.learningmarkets.com/determining-expectancy-in-your-trading/
  winrate <- winrate(x)
  winloss <- winloss(x, extreme=F)
  lossratio <- 1 - winrate
  #   winrate*mean(x[x>0]) + (1-winrate)*mean(x[x<0])
  winrate * winloss - lossratio
}

profitfactor <- function(x, extreme=F) {
  winrate <- winrate(x)
  winloss <- winloss(x, extreme=extreme)
  winloss * winrate / (1 - winrate)
}