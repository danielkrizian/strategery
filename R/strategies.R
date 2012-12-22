load.strategy <- function(strategy.name)
{
    file.name <- paste(strategy.name, 'RData', sep='.')

    load(file=file.name, envir=.strategy)
    assign(.strategy$strategy$name, .strategy$strategy, envir=.strategy)
}

# save a strategy object from memory onto disk
save.strategy <- function(strategy.name)
{
    strategy <- get(as.character(strategy.name), pos=.strategy, inherits=TRUE)
    file.name <- paste(strategy.name, 'RData', sep='.')

    save(strategy, pos=.strategy, file=file.name)
}

init.strategy <- function (strategy) {
  # initialize default rule function
  FUNname <- paste("rule",strategy,sep=".")

  add.signal(strategy,
             name=FUNname,
             arguments=list(),
#              parameters=as.list(formals(match.fun(FUNname))),
             label="signal",
             indexnum="signal",
             store=T)
  
  
}