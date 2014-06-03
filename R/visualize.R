#' Verify visually how indicators and signals did get computed
#' 
#' If no items are passed via ... argument, following auto treatment is applied:
#' \enumerate{
#'    \item Price indicator first: Indicator containing keywords 'Price', 'Close'
#'    if exists, is reserved for the first (top) graph.
#'    \item Similar indicators (up to 5) : similar indicators using 
#'    \code{overlap_rate} are detected and placed in the same top graph.
#'    \item Discrete indicator: is included in the top graph underlay
#'    \item Next graphs: Add items to the next graph. Loop the above procedure 
#'    until all indicators are used.
#' }
#' @export
Vis <- Visualise <- Visualize <- function(...,
                                          ids,
                                          window){
  inds = list(...)
  all <- ls(envir=parent.frame())
  lnames <- all[
    unlist(sapply(all, function(x) {
      o = get(x)
      return(inherits(o, "sfl") | inherits(o, "Indicator"))
    }, USE.NAMES=TRUE, simplify=FALSE))
    ]
  
  lind <- lapply(lnames, function(n) {
    o = get(n)
    if(is.sfl(o)) {
      ind = eval.sfl(o)
#     } else if(is.rule(o)) {
#       if(is.sfl(o$signal))
#         ind = eval.sfl(o$signal)
    } else if(is.indicator(o)) {
      ind = o
    }
    return(ind)
  })
  
  if(!missing(ids)) {
    i=0
    for(ind in lind) {
      i = i + 1
      lind[[i]] = ind[, ids]
    }
  }

  if(!missing(window)) {
    i=0
    for(ind in lind) {
      i = i + 1
      lind[[i]] = ind[window]
    }
  }
  
  ii = 0
  # first chart should be price chart, if there is a price indicator defined
  for(ind in lind) {
    ii = ii + 1
    if(grepl("close|price|cl",as.character(ind$trans)[1], ignore.case=T)) {
      lchdata = list(ind$xts())
      lind[ii] <- NULL
      break
    } else {
      lchdata = list(eval.sfl(indicator(Close, data=OHLCV))$xts())
    }
  }
  
  groupbyids=F # regime
  # extract data from other indicators among new/existing charts 
  
  #   while(length(lind)) {
  ii = 0
  #     chi = 0
  for(ind in lind) {
    todo = T
    for(chi in 1:length(lchdata)) {
      d = ind$xts()
      ch = lchdata[[chi]]
      if(!groupbyids & NCOL(ch) <= 5 & is.univariate(ind) &
           similar.scale(ch, d, thres = 0.6)) {
        # add to this chart, if similar
        lchdata[[chi]] =  merge(ch, d)
        names(lchdata[[chi]]) = c(names(ch), names(d)) # because merge.xts messes names up
        todo = F
      }
    }
    if(todo)
      lchdata = c(lchdata, list(d))
  }
  
  a = lapply(lchdata, function(x) dygraph(data = x, sync = T))
  layout_dygraphs(a)
    

  
  BSSC <- function(x) {
    as.character(cut(x,breaks=c(-Inf,0,Inf), labels=c("Sell", "Buy")))
  }

}