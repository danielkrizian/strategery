# Howard Bandy: Modeling Trading System Performance
# p.192
# requirements for position sizing determination
# need: ruin boundary and retire boundary
# objective: max TWR, MaxDD capped, min time to reach TWR

###### USE ########
# HealthTest(tail(b$trades,40), test="mean")
# p.value <- rollapply(b$trades$return, w=20, FUN=HealthTest, test="mean")
# ts.plot(p.value)
# abline(h=0.1, col="red")
# sum(p.value<0.10)/length(p.value)

# RUNS TEST
# appropriate for high system accuracy > 60%
# r = number of runs (consecutive Wins or Losses)
# mean = (2 * W * L / n) + 1
# stdev = sqrt((mean - 1) * (mean - 2) / (n - 1))
# z_score = (r - mean) / stdev
# z from <-1.65 ; 1.65>
# rolling 20 obs. window of z_scores

# ACCURACY OF TRADES
# appropriate for high system accuracy
# make CDFs for accuracies ranging from 20-80%
# for example, 60% accuracy system to be taken offline when <4 W in the last 10 trades, <6 in last 15 trades, or <9 in the last 20 trades
# W - testing number of winning trades
# z_score= (W - n * w%) / sqrt(n * w% * l%)
# 5%z =-1,65; for W=8, w%=0.6, n=20, z=-1.826 ==> broken

# T-TEST
# Bandy, p. 266
# H0: means difference = 0
# use when 
# population distribution is normal, 
# n=15: sampling distribution is symmetric, unimodal, no outliers
# n=16-40: sampling distribution moderately skewed, unimodal, no outliers
# n=41+: no outliers
# reference: Harry O. Posten (1978, 1979), Pearson, Please (1975)
# if above not met, then transform: Winsorizing (trim outliers) or take log (make closer to normal)
# t = (mean - tested mean) / (s / sqrt(n))
# CRITICAL: 1.66 (at 5%), 2.36 (at 1%) - both at 30 df

##### CHI-SQUARE
# Bandy, p. 266
# use when non-normal, in lieu of t-test
# bin data: equal-prob bins or equal-width bins; prefer equal-prob; min 5 data points per bin; 
# recommended number of bins: k = 1+ log2 N (N=data elements)
# recommended 2: bin size = 0.3 * StDev(sample data)
# chi2 = SUM FOR EACH BIN (observed - expected frequency for a bin)^2  / expected frequency for a bin
# df = bins - 1 ???   5% = 11.07, 10% = 9.24, 20% = 7.29
# naive test in Bandy showed slower lag vs t-test (25 trades vs 12 trades)


HealthTest <- function
(
  strategy,
  pars,
  dates=NULL,
  portfolio=Test(strategy,pars,dates=dates,
                 details=rtype=="trades",
                 returns=rtype=="periods"),
  R=Returns(portfolio, type=rtype[1], reduce=T),
  rtype=c("periods","trades"),
  test=c("runs","accuracy","mean"),
  w=0.6, # expected accuracy of trades. Used in accuracy test
  mu=c(0, "Random") # tested mean return
){

  test <- test[1]
                  
  n <- length(R)

  if(test=="runs") {
    if(sum(R >= 0)/n < 0.6)
      warning("Test is not appropriate for systems with accuracy < 60% ")
    W <- sum(R > 0)
    L <- sum(R < 0)
    nRuns <- length(rle(R > 0)$lengths) # number of runs
    E <- 1 + 2*W*L/n
    s2 <- (E-1)*(E-2)/(n-1) # = (2*W*L*(2*W*L-W-L))/((W+L)^2*(W+L-1))
    statistic <- (nRuns - E)/sqrt(s2)
    p.value <- pnorm(statistic) # H0: zero or negative auto-correlation
  }
  if(test=="accuracy") {
    if(sum(R >= 0)/n < 0.6)
      warning("Test is not appropriate for systems with accuracy < 60% ")
    W <- sum(R >= 0)
    statistic <- (W - n*w)/sqrt(n*w*(1-w))
    p.value <- pnorm(-statistic) # H0: observed accuracy <= w
  }
  if(test=="mean") {
    if(mu=="Random")
      mu = Benchmark(type="Random", portfolio=portfolio, dates=dates)
    muObs = "todo"

    s <- sd(x, na.rm=T)
    statistic = (muObs - mu) / (s / sqrt(length(x)))
    p.value <- pnorm(-statistic)
    
    return( Test.mean(R,mu=mu) )
  }

}


Test.mean <- function(x,mu=0) {

    p.value
#     p.value <- t.test(x, alternative="greater")$p.value # slightly different
}