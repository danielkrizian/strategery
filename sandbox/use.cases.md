


Strategy Specification, Backtesting, Statistical Analysis and Optimization Workflow Mock-Up
========================================================

#### Overview

The workflow consists mainly of defining the following variables as `quote()`-ed expressions to be `eval()`-uated later within the `Backtest()` function environment.

Main variables and functions:

1. `Param()`
2. `Buy <- quote(your condition to enter long position)`
3. `Sell <- quote(your condition to exit long position)`
4. `Short <- quote(your condition to enter short position)`
4. `Cover <- quote(your condition to exit short position)`
5. `PositionSize <- quote(your condition for money management)`
6. `PositionScore <- quote(your condition for ranking instruments)`
7. `Backtest()`
8. `Optimize()`

Main data management workhorse is data.table package (compared to xts for other backtesting packages).  
Package will be mainly working with vectors such as raw returns, conditional returns, signals in the form of vector of zeroes and ones;  position sizing expressed as a fraction of equity (allocation weights).  
It is thus return-oriented framework, not a transaction-oriented framework.

#### General Backtest Settings

Specify whether to trade on a next bar with lag.  
Specify time filter when the strategy trades (basic examples: at Open prices or Close prices)


```r
options(buydelay=1, selldelay=1, shortdelay=1, coverdelay=1
        , buyprice="Close", sellprice="Close", shortprice="Close", coverprice="Close")
```


#### Market Universe


```r
require(strategery)
data(SPX)
```



```r
require(strategery)
data(SPX)
```


See what tradable instrument data is available (markets universe) from your prefered data source.

```r
showInstruments()
```

Select the markets universe from the available instruments. This can be reset for each strategy separately later.

```r
options(instruments=c("SPY", "AAPL"))
```


1. Buy & Hold Strategy (Strategic Asset Allocation)
-------------------------------------------------------


### 1.1 No Rebalancing (Value-Weighted)


```r
newStrategy(name="BuyHold SPY and AAPL"
            , label="BuyHold")
```



```r
Buy <- quote(1)
```



Default position size 1 unit is used for each symbol 
from the selected universe at the initial buy. No rebalancing throughout the backtest.


```r
Backtest()
saveStrategy()
```


### 1.2 Rebalancing Monthly, Equal-Weighted


```r
newStrategy(name="BuyHold SPY and AAPL, rebalance monthly to equal weights"
            , label="BuyHold.m.e")
```


`Buy`, `Sell`, `Short`, `Cover` expressions from previous version of buy & hold strategy remain in the memory, so no need to repeat calls here.  Only the position sizing weighting scheme changes. Without call to *PositionSizing*, the default position size is 1 unit of each instrument.

Introducing now the portfolio-level position sizing.  
`WeightScheme()` function will create `data.table` object with weights for each instrument, timed and sized according to a specified scheme.


```r
PositionSize <- quote(WeightScheme(freq="monthly", method="equal"))
```



```r
Backtest()
saveStrategy()
```



### 1.3 Rebalancing Weekly. Custom Static Weights


```r
newStrategy(name="BuyHold SPY and AAPL, rebalance weekly to equal weights"
            , label="BuyHold.w.e")
```


`Buy`, `Sell`, `Short`, `Cover` expressions for previous version of buy & hold strategy remain in the memory, so again, no need to repeat calls here.

Introducing now the parametrization of inputs. 
To be used in the parameter optimization process to improve performance of the strategy. 


```r
Param (name="SPY Weight"
       , label="w.SPY"
       , default=70
       , min=0
       , max=0
       , step=5
       )
Param (name="AAPL Weight"
       , label="w.AAPL"
       , default=30
       , min=0
       , max=0
       , step=5
       )
PositionSize <- quote(WeightScheme(freq="weekly", w=c(w.SPY,w.AAPL)))
```


Alternatively:


```r
ParamList (name="Asset Weights"
           , label="weights"
           , list=list( "w.SPY"= c(70,0,0,5)
                       ,"w.AAPL"=c(30,0,0,5)))
PositionSize <- quote(WeightScheme(freq="weekly", w=weights))
```




```r
Backtest()
saveStrategy()
```


2. Tactical Asset Allocation
-------------------------------------------------------

`Buy`, `Sell`, `Short`, `Cover`

### 2.1 Dynamic Weights - Discretionary

Weights come from a precalculated source. Independent of backtest result.  


```r
weightDT <- data.table(Instrument=c("SPY","SPY","AAPL","AAPL")
                 , as.IDate(c("2000-01-01","2000-01-02","2000-01-01","2000-01-02"))
                 , W=c(0.1,0.2,0.3,0.4))
PositionSize <- quote(weightDT)
```


We can also run optimization by trying different sets of weights:


```r
ParamTable(name="Asset Weights"
           , label="weightDT"
           , sets=list(weightDT1, weightDT2))
PositionSize <- quote(weightDT)
```


Assets can be for example weighted by their rolling volatilities (risk-parity)


```r
PositionSize < quote( 0.02 * BuyPrice/(2*ATR(10)) ) #2% weight adjusted by volatility (Van Tharp-style)
```



```r
Backtest()
saveStrategy()
```


### 2.2 Dynamic Weights - Model-Driven 

Define any `Buy`, `Sell`, `Short`, `Cover` expressions here.

Now for example, path-dependent position sizing based on realized individual equity curve, bar-by-bar:


```r

myRiskBudgetFun <- function(x) {
  if(drawdown(x)>0.2) # if equity experiences severe drawdown..
    return(0.5)               # .... reduce model activity to 50%
}

PositionSize <- quote(0.02 * myRiskBudgetFun(Equity)) # Equity is internal reserved array of individual security equity curve

Backtest()
saveStrategy()
```


3. Trading Strategies (Buy/Sell/Short/Cover Signals To Enter/Exit Trades)
-------------------------------------------------------

Result is individual equity curve per symbol as if each instrument trades 1 unit. It is then up to position sizing expression in the `PositionSize` object to define weighting scheme and combine instruments into portfolio.


```r
newStrategy(name="Simple moving average system on SPY and AAPL", label="SMA")
```



```r
Param (name="MA window"
       , label="n"
       , default=100
       , min=5
       , max=500
       , step=5
       )
```



```r
require(TTR)
Buy  <- quote( Cross( Close        , SMA(Close, n) )) # Sell when Close Price crosses above its simple moving average
Sell <- quote( Cross( SMA(Close, n), Close         )) # Sell when Close Price crosses below its simple moving average
```



```r
Backtest()
saveStrategy()
```



4. Security Selection - Portfolio Screens & Ranks
-------------------------------------------------------

**Note:** Includes rotational/fund-switching systems, long-only or long-short hedged portfolios, market-neutral, relative value, stat arb (pair trading) strategies.  
The common mechanics of each of these styles is to *rank (score)* securities based on their attractiveness according to some statistic and *select* securities based on this rank.

Modes mentioned sofar use buy/sell/short/cover signals to enter/exit trades, while "rotational" mode (aka "ranking / switching" system) uses only position score.


```r
options(instruments=getUniverse("S&P500"))
```



```r
newStrategy(name="Long top decile low Price-to-earnings ratio, short bottom decile"
            , label="lowPE")
```


Take long position in the top 10th percentile, take short position in the bottom 10th percentile

```r
options (PercentileHeld=10)
```


Rank instruments based on their PE ratio (here obtained by custom function `getPE`)

```r
require(TTR)
PositionScore <- quote(getPE())
```



```r
Backtest()
saveStrategy()
```






```r
Backtest()
saveStrategy()
```

