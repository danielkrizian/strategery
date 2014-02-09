# signal <- function(x, y) {
#   #   if(is.indicator(x))
#   
#   #   if(is.signal(x))
#   
#   signal <- list()
#   signal$indicator <- deparse(substitute(x))
#   
#   if(is.numeric(y) || is.indicator(y))
#     threshold <- y
#   call <- quote(get(ind)[Value==y][,list(Instrument, Date)])
#   structure(signal, call=call, indicatorclass="signal")
# }

# binary_signal <- function(op, input, thres)  {
#   structure(list(op=op, input=input, thres=thres)
#             , class="binary_signal")
# }

# calc.signal <- function(x, ...) {
#   browser()
#   sig <- eval(x$call)
#   return(sig)
# #   ind.name <- get(deparse(x$input))
# #   ind.data <- calc(ind.name)
# #   thres <- eval(x$thres)
# #   op <- as.name(x$op)
# #   .call <- substitute(op(Value, thres))
# #   if(!is.data.table(thres)) {
# #     # does not assign directly Value:=eval(.call) because RHS is always logical 
# #     # in binary signals. Value:=eval(.call) throws data.table error: 
# #     # "Type of RHS ('logical') must match LHS."
# #     sig.data <- ind.data[,Signal:=eval(.call), by=Instrument]
# #     sig.data[,Value:=NULL]
# #   }
# #   return(sig.data)
# }

# calc.binary_signal <- function(x, ...) {
#   ind.name <- get(deparse(x$input))
#   ind.data <- calc(ind.name)
#   thres <- eval(x$thres)
#   op <- as.name(x$op)
#   .call <- substitute(op(Value, thres))
#   if(!is.data.table(thres)) {
#     # does not assign directly Value:=eval(.call) because RHS is always logical 
#     # in binary signals. Value:=eval(.call) throws data.table error: 
#     # "Type of RHS ('logical') must match LHS."
#     sig.data <- ind.data[,Signal:=eval(.call), by=Instrument]
#     sig.data[,Value:=NULL]
#   }
#   return(sig.data)
# }

signal <- function(call, data)  {
  structure(list(call=call, data=data)
            , class="signal")
}

ls_signals <- function (envir=.GlobalEnv) {
  all <- ls(envir=envir)
  all[sapply(all, function(x) class(get(x))[1] == "signal")]
}

calc.signal <- function(x, name, ...) {
  sig <- eval(x$call)
  if(!missing(name)) {
    if(is.symbol(name)) name <- deparse(name)
    setnames(sig$data, "Signal", name )
  }  
  return(sig)
}

data.signal <- function(x, name) {
  calc.signal(x, name=name)$data
}

print.signal <- function(x, ...) {
  sig <- calc(x, with.name=FALSE)
  print(sig$data)
}

#' x is a data.table Uninformative values are removed: NAs and repetitive values. Only
#' the leading signal is retained.
sig.compress <- function(x) {
  
  has.changed <- function(x) {
    ch <- c(TRUE,diff(x))
    ch <- as.logical(ch)
  }
  
  x <- x[x[!is.na(Signal),list(Instrument,Date)],] # remove NAs
  x[has.changed(Signal), list(Date, Signal), by=Instrument]
  x[, Signal:=ifelse(has.changed(Signal),Signal,NA), by= Instrument]
  x <- x[x[!is.na(Signal),list(Instrument,Date)],]
  return(x)
}

`Ops.signal` <- function(x, y) {
    op <- as.name(.Generic)
    return(signal(call = substitute(op(x, y)), data=NULL))
}


`!.signal` <- function(x) {
  op <- as.name(.Generic)
  .call <- substitute(op(x))
  
  x <- eval(x$call)
  .formula <- quote(!Signal)
  data <- x$data[, Signal:=eval(.formula)]
  
  return(signal(call = .call, data=data))
}

`%AND%` <- function(x, ...) {
  UseMethod("%AND%",x)
}

`%AND%.signal` <- function(x, y) {
  # TODO: test if setkey is necessary after rbind
  op <- as.name(.Generic)  
  .call <- substitute(op(x, y))
#   X <- eval(x$call)$data # used to be sig.compress
#   Y <- eval(y$call)$data # used to be sig.compress

  X <- calc.signal(x, name="Signal.x")$data
  Y <- calc.signal(y, name="Signal.y")$data
  
  Z <- unique(setkey(rbind(X[Y, roll = T, rollends=FALSE],
                           Y[X, roll = T, rollends=FALSE],
                           use.names = TRUE),
                     Instrument, Date)
              )[,Signal := as.logical(Signal.x * Signal.y)]
  
  Z <- Z[, list(Instrument, Date, Signal)]
  setkey(Z, Instrument, Date) #  delete line after upgrading data.table beyond rev. 999
#   Z <- sig.compress(Z)
  
  return(signal(call = .call, data=Z))
}

`%OR%` <- function(x, ...) {
  UseMethod("%OR%",x)
}

`%OR%.signal` <- function(x, y) {
  # TODO: test if setkey is necessary after rbind
  op <- as.name(.Generic)  
  .call <- substitute(op(x, y))
  #   X <- eval(x$call)$data # used to be sig.compress
  #   Y <- eval(y$call)$data # used to be sig.compress
  
  X <- calc.signal(x, name="Signal.x")$data
  Y <- calc.signal(y, name="Signal.y")$data
  
  Z <- unique(setkey(rbind(X[Y, roll = T, rollends=FALSE],
                           Y[X, roll = T, rollends=FALSE],
                           use.names = TRUE),
                     Instrument, Date)
  )[,Signal := as.logical(Signal.x + Signal.y)]
  
  Z <- Z[, list(Instrument, Date, Signal)]
  setkey(Z, Instrument, Date) #  delete line after upgrading data.table beyond rev. 999
  #   Z <- sig.compress(Z)
  
  return(signal(call = .call, data=Z))
}

Check <- function(Instrument="SPX"
                  , window=paste("2000",Sys.Date(),sep="::")
                  , plot=F){
  
  indnames <- ls_indicators()
  signames <- ls_signals()
  rulenames <- ls_rules()
  
  DT <- rbindlist(lapply(as.list(c(indnames, rulenames)), function(x) dat(get(x))[,Variable:=x] ))
  setkey(DT, Instrument, Date)

  DT <- DT[Instrument==Instrument]
  
  DT[, Title:=as.character(NA)]
  DT[, Annotation:=as.character(NA)]
  
  for (signame in signames) {
    sig <- dat.signal(get(signame))
    DT <- DT[Date %between% range(sig$Date) ]
    DT[sig, Title:= paste(ifelse(is.na(Title),"", paste(Title, ",", sep=" "))
                          , signame), mult="first"]
    DT[sig, Annotation:= paste(ifelse(is.na(Annotation)
                                      ,"", paste(Annotation, ",", sep=" ")) 
                               , paste(signame, sig$Signal, sep=" = ")), mult="first"]
  }
  
  DT <- DT[Date %between% rangeISO8601(window),]
  
if(plot) {
  library(googleVis)
  gv <- gvisAnnotatedTimeLine(DT, datevar="Date",
                              numvar="Value", idvar="Variable",
                              titlevar="Title", annotationvar="Annotation",
                              #                             options=list(displayAnnotations=FALSE,
                              #                                          legendPosition='newRow',
                              #                                          width=600, height=350)
                              options=list(displayAnnotations=TRUE,
                                           #                                            zoomStartTime=as.Date(max(DT$Date))-100,
                                           #                                            zoomEndTime=as.Date(max(DT$Date)),
                                           #                                            colors="['blue', 'lightblue']",
                                           displayAnnotationsFilter=TRUE,
                                           width=1500, height=600, 
                                           displayExactValues= TRUE,
                                           scaleColumns='[0,1,2]',
                                           scaleType='allmaximized',
                                           wmode='transparent')
  )
  plot(gv)
}
  DT <- dcast.data.table(DT, Instrument+Date~Variable , value.var="Value", drop=TRUE)
  View(DT)
}

