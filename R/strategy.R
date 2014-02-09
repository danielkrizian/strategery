
#' @export
newStrategy <- function 
(name
 #, label
) 
{
  s <- list(name=name)
  class(s) <- "strategy"
  assign(name, s , envir=.GlobalEnv)
}

#' @export
saveStrategy <- function(
  # envir=strategy$name
)
{
  s <- get("strategy", envir=.GlobalEnv)
  s$backtest <- copy(R) # is it necessary to make copy?
  # c("Buy", "Sell", "Short", "Cover")
  s$Buy <- Buy
  s$Sell <- Sell
  s$Short <- Short
  s$Cover <- Cover
  assign(strategy$name, s , envir=.GlobalEnv)
}