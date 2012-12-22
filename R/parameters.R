
add.parameters <- 
      function (strategy, 
              type = 'signals', 
              add.to.name="signal",
              min=NULL ,
              max=NULL,
              step=1,
#               label = NULL,
              store=T) 
{
  
  s <- getStrategy(strategy)
  
  FUNname <- s[[type]][[add.to.name]]$name
  labels <- as.list(formals(match.fun(FUNname)))
  
  s$parameters <- list()
  for (l in 1:length(labels)) {
    distr <- sort(unique(c(pmax(min[l]:max[l] - (min[l]:max[l])%%step[l], min[l]),max[l])))
    s$parameters[[names(labels)[l]]] <- distr
  }

  if (store) assign(s$name, s, envir=as.environment(.strategy))
  else return(s)
}


getParBounds <- function(strategy) {
  s <- getStrategy(strategy)
  out <- list()
  out$lower <- unlist(lapply(s$parameters,min))
  out$upper <- unlist(lapply(s$parameters,max))
return(out)
}
