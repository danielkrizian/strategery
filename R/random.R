
#' Some Title
#' 
#' @export
RandomPortfolios <- function
(strategy
 ,pars
 ,n=100  # number of randomly generated portfolios
 ,parallel=T # compute in parallel
){
  # generate return series of n random portfolios

  #### base strategy ##
  to.permute <- Signals(strategy, pars)
#   timestamps <- index(BackTest.Params(ruleFUN=rule.rand, 
#                                       params=list(sig=to.permute)))
  baseR <- Test(strategy,pars)$R

  randomS <- strategy("random",assets=getStrategy(strategy)$assets,store=T)

  randomS <-  add.signal(randomS,
             name="rule.rand",
             arguments=list(),
#              parameters=as.list(formals(match.fun(FUNname))),
             label="signal",
             indexnum="signal",
             store=F)

  if(parallel) {
    cl <- createCluster(parVar=c("rule.rand","Test",
                              "Execute","Signals","market","portfolio","Returns")
                        ,packages=c("xts","quantstrat"))
    randR <- parSapply(cl=cl, 1:n,
                       function(iteration, pars) {
                         Rxts <- Test(randomS, pars=pars)$R
                         coredata(Rxts)
                       }
                       , pars=list(sig=to.permute), simplify=T)
  } else {
    randR <- sapply(1:n,
                    function(iteration, pars) {
                      Rxts <- BackTest(randomS, pars=pars)$R
                      coredata(Rxts)
                    }
                    , pars=list(sig=to.permute), simplify=T)
  }
  
  randR <- xts(randR, order.by=index(baseR))
  
  return(list(R=randR))
}

