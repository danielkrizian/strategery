#' One of our goals with an event-driven trading system is to minimise 
#' duplication of code between the backtesting element and the live execution 
#' element. Ideally it would be optimal to utilise the same signal generation 
#' methodology and portfolio management components for both historical testing 
#' and live trading. In order for this to work the Strategy object which 
#' generates the Signals, and the Portfolio object which provides Orders based 
#' on them, must utilise an identical interface to a market feed for both 
#' historic and live running. 
#' 
#' This motivates the concept of a class hierarchy based on a DataHandler 
#' object, which gives all subclasses an interface for providing market data to 
#' the remaining components within the system. In this way any subclass data 
#' handler can be "swapped out", without affecting strategy or portfolio 
#' calculation.
#' 
#' The DataHandler is an abstract base class (ABC), which means that it is 
#' impossible to instantiate an instance directly. Only subclasses may be 
#' instantiated. The rationale for this is that the ABC provides an interface 
#' that all subsequent DataHandler subclasses must adhere to thereby ensuring 
#' compatibility with other classes that communicate with them.
#' 
#' DataHandler-derived objects, are used by the remaining components to keep 
#' track of market data. The Strategy, Portfolio and ExecutionHandler objects 
#' all require the current market data thus it makes sense to centralise it to 
#' avoid duplication of storage.
#' 
#' The two methods of interest are get_latest_bars and update_bars. The former 
#' returns the last N bars from the current heartbeat timestamp, which is useful
#' for rolling calculations needed in Strategy classes. The latter method 
#' provides a "drip feed" mechanism for placing bar information on a new data 
#' structure that strictly prohibits lookahead bias. Notice that exceptions 
#' will be raised if an attempted instantiation of the class occurs.
#' @name test1
#' @import methods
#' @exportClass DataHandler
#' @field continue backtest logical
#' @include Event.R
DataHandler <- setRefClass("DataHandler", contains="VIRTUAL",
                           fields = list(
                             continue.backtest="logical"
                           ),
                           methods = list(
                             updateBars=function(){
  "Pushes the latest bar to the latest symbol structure for all symbols in the 
  symbol list.
  This is triggered when the outer while loop begins a new 'heartbeat'. It 
  occurs when the DataHandler object receives a new update of market data for 
  any symbols which are currently being tracked. The event object simply 
  contains an identification that it is a market event, with no other 
  structure. It is used to trigger the Strategy object generating new trading 
  signals."
                               stop("Should implement updateBars()")
                               return(list(type="market"))
                             },
                             
                             getLatestBars = function(symbol, N=1){
  "Returns the last N bars from the latest_symbol list, or fewer if less bars 
  are available."
                               stop("Should implement getLatestBars()")
                             }
                           )
)

#' historic CSV data handler, which will load intraday CSV data for equities 
#' in an Open-Low-High-Close-Volume-OpenInterest set of bars. 
#' This can then be used to "drip feed" on a bar-by-bar basis the data 
#' into the Strategy and Portfolio classes on every heartbeat of the system, 
#' thus avoiding lookahead bias.
#' 
#' HistoricCSVDataHandler will take multiple CSV files, one for each symbol, and
#' convert these into an appropriate R substitute of dictionary of pandas 
#' DataFrames.
#' 
#' Designed to read CSV files for each requested symbol from disk and provide an
#' interface to obtain the "latest" bar in a manner identical to a live trading
#' interface.
#' @field events Event Queue object
#' @field dir Absolute directory path to the CSV files.
#' @field symbols A character vector of symbol strings.
#' @include Event.R
#' @import iterators
CSVDataHandler <- setRefClass("CSVDataHandler", contains="DataHandler",
                              fields = list(
                                events = "EventQueue",
                                dir = "character",
                                symbols = "character",
                                data = "data.table",
                                latest.data = "data.table"
                              ),
                              methods = list(
                                initialize = function(events, dir, symbols){
"Initialises the historic data handler by requesting the location of the CSV 
files and a list of symbols.

It will be assumed that all files are of the form 'symbol.csv', where symbol is 
a string in the list.

Parameters:
  events - The Event Queue.
  csv_dir - Absolute directory path to the CSV files.
  symbol_list - A list of symbol strings."
                                  events <<- events
                                  dir <<- dir
                                  symbols <<- symbols
                                  data <<- data.table()
                                  latest.data <<- data.table()
                                  continue.backtest <<- TRUE
                                  openConvertCSV()
                                },

                                updateBars = function(){
"Generates a MarketEvent that gets added to the queue as it appends the latest 
bars to the latest.data.
Pushes the latest bar to the latest.data structure for all symbols in the
symbol list."
require(iterators)
for(s in symbols) {
  tryCatch(bar = nextElem(getNewBar(s)),
           continue.backtest = FALSE
  )
  if(!is.null(bar))
    latest.data <<- rbindlist(latest.data, bar)
}

events$put(MarketEvent())
return(list(type="market"))
                                },

                                getLatestBars = function(symbol, N=1){
"Provides a list of the last N bars from the latest_symbol_data structure. 
Setting N=1 allows the retrieval of the current bar (wrapped in a list).

Returns the last N bars from the latest_symbol list, or N-k if less available."
                                  tryCatch(
                                    bars.list = latest.data[symbol],
                                    print("That symbol is not available in the historical data set.")
                                  )
                                  return (tail(bars.list,N))
                                },
                                
                                openConvertCSV = function(){
"Opens the CSV files from the data directory, converting them into 
pandas DataFrames within a symbol dictionary.
For this handler it will be assumed that the data is taken from DTN IQFeed. 
Thus its format will be respected.
Symbol indices are not aligned. TODO: consider na.fill as in the original."
                                  for(s in symbols){
                                    # Load the CSV file with no header information, indexed on date
                                    symbol.data = read.csv(
                                      file=file.path(dir, sprintf('%s.csv', s)),
                                      header=FALSE, 
                                      row.names=c('datetime','open','low',
                                                  'high','close','volume','oi')
                                    )
                                    data <<- rbindlist(data, 
                                                       as.data.table(symbol.data))
                                  }
                                },
                                
                                getNewBar = function(symbol){
"Creates a generator to provide a formatted version of the bar data. This means 
that subsequent calls to the method will yield a new bar until the end of the 
symbol data is reached.
Returns the latest bar from the data feed as a tuple of 
(sybmbol, datetime, open, low, high, close, volume)."
                          # http://www.exegetic.biz/blog/2013/11/iterators-in-r/
                                  require(iterators)
                                  for(b in data[symbol])
                                    idata = iter(b)
                                  return(idata)
                              # IMPLEMENTATION IN python:
                              # for b in data[symbol]:
                              #   yield tuple([symbol, strptime(b[0], '%Y-%m-%d %H:%M:%S'), 
                              #                b[1][0], b[1][1], b[1][2], b[1][3], b[1][4]])
                                 }
                              )
)