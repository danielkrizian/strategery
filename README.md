strategery
==========

Quant Strategy Specification, Backtesting, Optimization And Statistical Analysis Workflow

1. Data management (in real time and for backtesting purposes)
2. Signal generation system (create, buy and sell signals according to predefined strategies using quantitative methods)
3. Portfolio and P&L tracking system
4. Quantitative risk management system (simulate extreme values, limit orders to fit to VaR restrictions)
5. Routing and execution subsystem (execution trading algorithms, like TWAP, VWAP...)

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
