
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

####### DRAWDOWNS ######

# Calculate drawdowns
dd <- function(x) {
  e <- cumprod(1+x)
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
