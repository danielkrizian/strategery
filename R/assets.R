Assets.returns <- function(interval="days") {
  # Return = PL / abs(Prev.Value) # abs to accommodate short positions too
  if(length(.performance))
    performance <- .performance
  else {
    if(inherits(.self, "Portfolio")) {
      if(! "PL" %in% names(assets)) assets <<- .self$calcPL()
      
      performance <- assets[,list(PL=sum(PL), Prev.Value=sum(Prev.Value)), by=Date]
      performance[             , Return:=0]
      performance[Prev.Value!=0, Return:=PL/abs(Prev.Value)]
      performance[,Instrument:=if(length(name)) name else "Portfolio"]
    }
    if(inherits(.self, "Account") & !is.null(.self$benchmarks)) {
      
    }
  }
  
  performance <- performance[,list(Instrument, Date, Return)] 
  #                           setkey(performance, Instrument, Date) # "Date" key lost here
  class(performance) <- c("returns", class(performance))
  return(performance)
}

Assets.performance <- function() {
  performance = .self$returns()
  performance[,Equity:=cumprod(1+Return), by=Instrument]
  performance <- performance[,Drawdown:=dd(Return), by=Instrument]
  performance
}

Assets <- setRefClass("Assets"
                      , fields = list(name="character"
                                      , assets = "data.table"
                                      # function(v) {
#                                         # usually has fields Return, NAV
#                                         if(missing(v)) {
#                                           
#                                           if(inherits(.self,"Account")) {
#                                             .self$NAV()
#                                           }
#                                           
#                                           return(.performance)
#                                         }
#                                         assign('.performance',v,.self)
#                                       }
#                                       , prices="data.table" # to .perf
#                                       , returns="data.table" # to .perf
                                      , info="data.frame"
                      )
                      , methods = list(
                        
                        initialize=function(...)  {                        
                          assign('.performance', data.table(), .self)
                          .self$initFields(...)
                        },
                        
                        returns=Assets.returns,
                        
                        prices=function(interval="days", base=100) {
                          if(length(.performance))
                            performance <- .performance
                          else {
                            performance <- returns(interval=interval)
                            if(inherits(.self, "Portfolio")) {
                              performance[, Index:=base*cumprod(1+Return)]
                            }
                          }
                          class(performance) <- c("prices", class(performance))
                          return(performance)
                        },
                        
                        performance=Assets.performance)
)


##### EXPERIMENTAL #####

#' constructor for creating an portfolio object.
assets <- function(symbols){
  
}
#' TODO: vectorize pattern
Search <- function(pattern, category="instrument") {
  View(instrument.table(find.instrument(pattern)))
}

