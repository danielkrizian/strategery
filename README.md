strategery
==========

## Quant Strategy Specification, Backtesting, Optimization And Statistical Analysis Workflow

Strategy development process must be efficient! Large chunk of quants' time spent in data preparation, not in analysis. Decouple data processing and analytics. Minimizing time-to-value requires: 
* idea has to be easily expressible and sharable with other people; machines need to understand it too (DSL)
* variety and volume of available data 
*	efficient queries (easy to write, quick response)
* interactive ad-hoc analysis (R, visualization)

1. Data management (in real time and for backtesting purposes)
2. Signal generation system (create, buy and sell signals according to predefined rules)
3. Visualisation and statistical validation of rules
4. Portfolio and P&L tracking system
5. Routing and execution subsystem (execution trading algorithms, like TWAP, VWAP...)

## Trading Rules
A `rule` is a chain of operations that transform `rule`'s input (raw market or other data) into `rule`'s output: recommended market position (long/short/neutral).
`rule` is defined by one or more mathematical and logical operators.
A `rule` is said to generate a "signal" when the value of the output series (i.e. position series) changes.
	
    indicator1 = rawdata %transform% FUN()
    ....
    signal = indicator %binary.operator% threshold
    position = signal %positionsizing% FUN()

## Getting Started

Install with:

    library("devtools")
    install_github("strategery", "danielkrizian")

See `examples` folder for use cases like:

    require(strategery)
    
    # This strategy goes long the S&P 500 index around the turn of each month 
    # (last trading day and first three trading days). 
    # Momentum filter applied, i.e. price must be greater than 20 days ago.
    newStrategy("tom") 

    # select universe
    symbols <- "SPX"
    Universe(symbols) # prepare ohlc data frame

    cal <- time.frame(symbols, bds=TRUE) # trading days calendar

    TurnOfMonth <- function(x, last=1, first=3, advance=2) {
      eom <-endpoints(x, on="months") # end of month
      tom <- sort(as.vector(outer(eom, (-last+1 - advance):(first - advance),"+"))) # shift backward&forward
      tom <- tom[tom > 0 & tom <= length(x)] # eliminate values outside range
      1:length(x) %in% tom
    }

    nmom <- 20
    last.days <- 1
    first.days <- 3

    TOM <- indicator( TurnOfMonth(Date, last.days, first.days, advance=2), input=cal)
    mom <- indicator( momentum(Close, nmom), input=OHLCV)

    Long <- (mom>0) %AND% (TOM==TRUE) %position% shares(1) # %buy% equity.pct(2) 
    Neutral <- (mom<=0) %OR% (TOM!=TRUE) %position% shares(0)
    
    Check(plot=T, window="1980")

    Backtest()
