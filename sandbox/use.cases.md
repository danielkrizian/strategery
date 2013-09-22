


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
options(key=c("Instrument","idate"))
options(BuyPrice=data.table(Instrument="SPX"
                            , idate=as.IDate(index(SPX))
                            , BuyPrice=as.vector(SPX$SPX.Close)
                            , key=c("Instrument","idate")))
```

```
## Error: could not find function "data.table"
```

```r
options(BuyPrice=quote(Close), SellPrice=quote(Open), CoverPrice=quote(Open), ShortPrice=quote(Open))
options(TradeDelays=list(Buy=1,Sell=1,Short=1,Cover=1))
```


#### Market Universe


```r
require(strategery)
require(data.table)
require(xts)
data(SPX)
```


See what tradable instrument data is available (markets universe) from your prefered data source.

```r
showInstruments()
```

```
## [1] "SPX"
```


1. Buy & Hold Strategy
-------------------------------------------------------


### 1.1.1 Single-Asset

Select the markets universe from the available instruments. This can be reset for each strategy separately later.

```r
options(instruments=c("SPX"))
```



```r
newStrategy(name="BuyHold SPX"
            , label="BuyHold")
```



```r
Buy  <- quote(TRUE)
Sell <- quote(FALSE)
```



Default position size 1 unit is used for each symbol 
from the selected universe at the initial buy. No rebalancing throughout the backtest.


```r
Backtest()
```

```
##        Instrument      idate    Open   Close        Raw   Buy  Sell Short Cover BuyPrice SellPrice ShortPrice CoverPrice Pos     Return
##     1:        SPX 1927-12-30   17.66   17.66         NA    NA    NA    NA    NA    17.66     17.66      17.66      17.66   0         NA
##     2:        SPX 1928-01-03   17.76   17.76  0.0056465  TRUE FALSE FALSE FALSE    17.76     17.76      17.76      17.76   1  0.0000000
##     3:        SPX 1928-01-04   17.72   17.72 -0.0022548 FALSE FALSE FALSE FALSE    17.72     17.72      17.72      17.72   1 -0.0022548
##     4:        SPX 1928-01-05   17.55   17.55 -0.0096400 FALSE FALSE FALSE FALSE    17.55     17.55      17.55      17.55   1 -0.0096400
##     5:        SPX 1928-01-06   17.66   17.66  0.0062482 FALSE FALSE FALSE FALSE    17.66     17.66      17.66      17.66   1  0.0062482
##     6:        SPX 1928-01-09   17.50   17.50 -0.0091013 FALSE FALSE FALSE FALSE    17.50     17.50      17.50      17.50   1 -0.0091013
##     7:        SPX 1928-01-10   17.37   17.37 -0.0074563 FALSE FALSE FALSE FALSE    17.37     17.37      17.37      17.37   1 -0.0074563
##     8:        SPX 1928-01-11   17.35   17.35 -0.0011521 FALSE FALSE FALSE FALSE    17.35     17.35      17.35      17.35   1 -0.0011521
##     9:        SPX 1928-01-12   17.47   17.47  0.0068926 FALSE FALSE FALSE FALSE    17.47     17.47      17.47      17.47   1  0.0068926
##    10:        SPX 1928-01-13   17.58   17.58  0.0062768 FALSE FALSE FALSE FALSE    17.58     17.58      17.58      17.58   1  0.0062768
##    ---                                                                                                                                 
## 21331:        SPX 2012-12-05 1407.05 1409.28  0.0015836 FALSE FALSE FALSE FALSE  1409.28   1407.05    1407.05    1407.05   1  0.0015836
## 21332:        SPX 2012-12-06 1409.43 1413.94  0.0033012 FALSE FALSE FALSE FALSE  1413.94   1409.43    1409.43    1409.43   1  0.0033012
## 21333:        SPX 2012-12-07 1413.95 1418.07  0.0029167 FALSE FALSE FALSE FALSE  1418.07   1413.95    1413.95    1413.95   1  0.0029167
## 21334:        SPX 2012-12-10 1418.07 1418.55  0.0003384 FALSE FALSE FALSE FALSE  1418.55   1418.07    1418.07    1418.07   1  0.0003384
## 21335:        SPX 2012-12-11 1418.55 1427.84  0.0065276 FALSE FALSE FALSE FALSE  1427.84   1418.55    1418.55    1418.55   1  0.0065276
## 21336:        SPX 2012-12-12 1427.84 1428.48  0.0004481 FALSE FALSE FALSE FALSE  1428.48   1427.84    1427.84    1427.84   1  0.0004481
## 21337:        SPX 2012-12-13 1428.48 1419.45 -0.0063415 FALSE FALSE FALSE FALSE  1419.45   1428.48    1428.48    1428.48   1 -0.0063415
## 21338:        SPX 2012-12-14 1419.45 1413.58 -0.0041440 FALSE FALSE FALSE FALSE  1413.58   1419.45    1419.45    1419.45   1 -0.0041440
## 21339:        SPX 2012-12-17 1413.54 1430.36  0.0118007 FALSE FALSE FALSE FALSE  1430.36   1413.54    1413.54    1413.54   1  0.0118007
## 21340:        SPX 2012-12-18 1430.47 1446.79  0.0114211 FALSE FALSE FALSE FALSE  1446.79   1430.47    1430.47    1430.47   1  0.0114211
```

```r
saveStrategy()
```

```
## NULL
```


### 1.1.2 Multi-Asset (Strategic Asset Allocation). No Rebalancing (Value-Weighted)


```r
options(instruments=c("SPY","AAPL"))
```



```r
newStrategy(name="BuyHold SPY and AAPL"
            , label="BuyHold")
```



```r
Buy  <- quote(TRUE)
Sell <- quote(FALSE)
```



Default position size 1 unit is used for each symbol 
from the selected universe at the initial buy. No rebalancing throughout the backtest.


```r
Backtest()
```

```
##        Instrument      idate    Open   Close        Raw   Buy  Sell Short Cover BuyPrice SellPrice ShortPrice CoverPrice Pos     Return
##     1:        SPX 1927-12-30   17.66   17.66         NA    NA    NA    NA    NA    17.66     17.66      17.66      17.66   0         NA
##     2:        SPX 1928-01-03   17.76   17.76  0.0056465  TRUE FALSE FALSE FALSE    17.76     17.76      17.76      17.76   1  0.0000000
##     3:        SPX 1928-01-04   17.72   17.72 -0.0022548 FALSE FALSE FALSE FALSE    17.72     17.72      17.72      17.72   1 -0.0022548
##     4:        SPX 1928-01-05   17.55   17.55 -0.0096400 FALSE FALSE FALSE FALSE    17.55     17.55      17.55      17.55   1 -0.0096400
##     5:        SPX 1928-01-06   17.66   17.66  0.0062482 FALSE FALSE FALSE FALSE    17.66     17.66      17.66      17.66   1  0.0062482
##     6:        SPX 1928-01-09   17.50   17.50 -0.0091013 FALSE FALSE FALSE FALSE    17.50     17.50      17.50      17.50   1 -0.0091013
##     7:        SPX 1928-01-10   17.37   17.37 -0.0074563 FALSE FALSE FALSE FALSE    17.37     17.37      17.37      17.37   1 -0.0074563
##     8:        SPX 1928-01-11   17.35   17.35 -0.0011521 FALSE FALSE FALSE FALSE    17.35     17.35      17.35      17.35   1 -0.0011521
##     9:        SPX 1928-01-12   17.47   17.47  0.0068926 FALSE FALSE FALSE FALSE    17.47     17.47      17.47      17.47   1  0.0068926
##    10:        SPX 1928-01-13   17.58   17.58  0.0062768 FALSE FALSE FALSE FALSE    17.58     17.58      17.58      17.58   1  0.0062768
##    ---                                                                                                                                 
## 21331:        SPX 2012-12-05 1407.05 1409.28  0.0015836 FALSE FALSE FALSE FALSE  1409.28   1407.05    1407.05    1407.05   1  0.0015836
## 21332:        SPX 2012-12-06 1409.43 1413.94  0.0033012 FALSE FALSE FALSE FALSE  1413.94   1409.43    1409.43    1409.43   1  0.0033012
## 21333:        SPX 2012-12-07 1413.95 1418.07  0.0029167 FALSE FALSE FALSE FALSE  1418.07   1413.95    1413.95    1413.95   1  0.0029167
## 21334:        SPX 2012-12-10 1418.07 1418.55  0.0003384 FALSE FALSE FALSE FALSE  1418.55   1418.07    1418.07    1418.07   1  0.0003384
## 21335:        SPX 2012-12-11 1418.55 1427.84  0.0065276 FALSE FALSE FALSE FALSE  1427.84   1418.55    1418.55    1418.55   1  0.0065276
## 21336:        SPX 2012-12-12 1427.84 1428.48  0.0004481 FALSE FALSE FALSE FALSE  1428.48   1427.84    1427.84    1427.84   1  0.0004481
## 21337:        SPX 2012-12-13 1428.48 1419.45 -0.0063415 FALSE FALSE FALSE FALSE  1419.45   1428.48    1428.48    1428.48   1 -0.0063415
## 21338:        SPX 2012-12-14 1419.45 1413.58 -0.0041440 FALSE FALSE FALSE FALSE  1413.58   1419.45    1419.45    1419.45   1 -0.0041440
## 21339:        SPX 2012-12-17 1413.54 1430.36  0.0118007 FALSE FALSE FALSE FALSE  1430.36   1413.54    1413.54    1413.54   1  0.0118007
## 21340:        SPX 2012-12-18 1430.47 1446.79  0.0114211 FALSE FALSE FALSE FALSE  1446.79   1430.47    1430.47    1430.47   1  0.0114211
```

```r
saveStrategy()
```

```
## NULL
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

### 3.1.1 Simple Moving-Average Crossover(Single-Market)

Select the markets universe from the available instruments. This can be reset for each strategy separately later.

```r
options(instruments=c("SPX"))
showInstruments()
```

```
## [1] "SPX"
```



```r
Buy <- quote(Cross(Close, SMA(Close,n)))
Sell <- quote(Cross(SMA(Close,n), Close))
```



```r
n<-5
```



```r
AddColumn( quote(SMA(Close, n)), paste("SMA",n))
```

```
##        Instrument      idate    Open   Close        Raw   SMA 5
##     1:        SPX 1927-12-30   17.66   17.66         NA      NA
##     2:        SPX 1928-01-03   17.76   17.76  0.0056465      NA
##     3:        SPX 1928-01-04   17.72   17.72 -0.0022548      NA
##     4:        SPX 1928-01-05   17.55   17.55 -0.0096400      NA
##     5:        SPX 1928-01-06   17.66   17.66  0.0062482   17.67
##     6:        SPX 1928-01-09   17.50   17.50 -0.0091013   17.64
##     7:        SPX 1928-01-10   17.37   17.37 -0.0074563   17.56
##     8:        SPX 1928-01-11   17.35   17.35 -0.0011521   17.49
##     9:        SPX 1928-01-12   17.47   17.47  0.0068926   17.47
##    10:        SPX 1928-01-13   17.58   17.58  0.0062768   17.45
##    ---                                                         
## 21331:        SPX 2012-12-05 1407.05 1409.28  0.0015836 1411.58
## 21332:        SPX 2012-12-06 1409.43 1413.94  0.0033012 1411.18
## 21333:        SPX 2012-12-07 1413.95 1418.07  0.0029167 1411.56
## 21334:        SPX 2012-12-10 1418.07 1418.55  0.0003384 1413.38
## 21335:        SPX 2012-12-11 1418.55 1427.84  0.0065276 1417.54
## 21336:        SPX 2012-12-12 1427.84 1428.48  0.0004481 1421.38
## 21337:        SPX 2012-12-13 1428.48 1419.45 -0.0063415 1422.48
## 21338:        SPX 2012-12-14 1419.45 1413.58 -0.0041440 1421.58
## 21339:        SPX 2012-12-17 1413.54 1430.36  0.0118007 1423.94
## 21340:        SPX 2012-12-18 1430.47 1446.79  0.0114211 1427.73
```

```r
Backtest()
```

```
##        Instrument      idate    Open   Close        Raw   SMA 5   Buy  Sell Short Cover BuyPrice SellPrice ShortPrice CoverPrice Pos     Return
##     1:        SPX 1927-12-30   17.66   17.66         NA      NA    NA    NA    NA    NA    17.66     17.66      17.66      17.66   0         NA
##     2:        SPX 1928-01-03   17.76   17.76  0.0056465      NA FALSE FALSE FALSE FALSE    17.76     17.76      17.76      17.76   0  0.0000000
##     3:        SPX 1928-01-04   17.72   17.72 -0.0022548      NA FALSE FALSE FALSE FALSE    17.72     17.72      17.72      17.72   0  0.0000000
##     4:        SPX 1928-01-05   17.55   17.55 -0.0096400      NA FALSE FALSE FALSE FALSE    17.55     17.55      17.55      17.55   0  0.0000000
##     5:        SPX 1928-01-06   17.66   17.66  0.0062482   17.67 FALSE FALSE FALSE FALSE    17.66     17.66      17.66      17.66   0  0.0000000
##     6:        SPX 1928-01-09   17.50   17.50 -0.0091013   17.64 FALSE  TRUE FALSE FALSE    17.50     17.50      17.50      17.50   0 -0.0090600
##     7:        SPX 1928-01-10   17.37   17.37 -0.0074563   17.56 FALSE FALSE FALSE FALSE    17.37     17.37      17.37      17.37   0  0.0000000
##     8:        SPX 1928-01-11   17.35   17.35 -0.0011521   17.49 FALSE FALSE FALSE FALSE    17.35     17.35      17.35      17.35   0  0.0000000
##     9:        SPX 1928-01-12   17.47   17.47  0.0068926   17.47 FALSE FALSE FALSE FALSE    17.47     17.47      17.47      17.47   0  0.0000000
##    10:        SPX 1928-01-13   17.58   17.58  0.0062768   17.45 FALSE FALSE FALSE FALSE    17.58     17.58      17.58      17.58   0  0.0000000
##    ---                                                                                                                                         
## 21331:        SPX 2012-12-05 1407.05 1409.28  0.0015836 1411.58 FALSE FALSE FALSE FALSE  1409.28   1407.05    1407.05    1407.05   0  0.0000000
## 21332:        SPX 2012-12-06 1409.43 1413.94  0.0033012 1411.18 FALSE FALSE FALSE FALSE  1413.94   1409.43    1409.43    1409.43   0  0.0000000
## 21333:        SPX 2012-12-07 1413.95 1418.07  0.0029167 1411.56  TRUE FALSE FALSE FALSE  1418.07   1413.95    1413.95    1413.95   1  0.0000000
## 21334:        SPX 2012-12-10 1418.07 1418.55  0.0003384 1413.38 FALSE FALSE FALSE FALSE  1418.55   1418.07    1418.07    1418.07   1  0.0003384
## 21335:        SPX 2012-12-11 1418.55 1427.84  0.0065276 1417.54 FALSE FALSE FALSE FALSE  1427.84   1418.55    1418.55    1418.55   1  0.0065276
## 21336:        SPX 2012-12-12 1427.84 1428.48  0.0004481 1421.38 FALSE FALSE FALSE FALSE  1428.48   1427.84    1427.84    1427.84   1  0.0004481
## 21337:        SPX 2012-12-13 1428.48 1419.45 -0.0063415 1422.48 FALSE FALSE FALSE FALSE  1419.45   1428.48    1428.48    1428.48   1 -0.0063415
## 21338:        SPX 2012-12-14 1419.45 1413.58 -0.0041440 1421.58 FALSE  TRUE FALSE FALSE  1413.58   1419.45    1419.45    1419.45   0  0.0000000
## 21339:        SPX 2012-12-17 1413.54 1430.36  0.0118007 1423.94 FALSE FALSE FALSE FALSE  1430.36   1413.54    1413.54    1413.54   0  0.0000000
## 21340:        SPX 2012-12-18 1430.47 1446.79  0.0114211 1427.73  TRUE FALSE FALSE FALSE  1446.79   1430.47    1430.47    1430.47   1  0.0000000
```

```r
saveStrategy()
```

```
## NULL
```

### 3.1.2 Simple Moving-Average Crossover (Multi-Market)


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

