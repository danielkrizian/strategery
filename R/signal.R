#' Verify visually how indicators and signals got evaluated
#' 
#' @export
Check <- function(instrument
                  , window=paste("2000",Sys.Date(),sep="::")
                  , plot=F){
  
  indnames <- ls_indicators()
  rulenames <- ls_rules()
  
  DT <- rbindlist(lapply(as.list(c(indnames, rulenames)), function(x) {
    o <- as.indicator(get(x))
    o[,Variable:=x]
  }
    ))
  setkey(DT, Instrument, Date)
  
  if(!missing(instrument))
  DT <- DT[J(Instrument=instrument)]
  DT <- DT[Date %between% range.ISO8601(window),]
  
  DT[, Title:=as.character(NA)]
  DT[, Annotation:=as.character(NA)]
  
  # add signals to annotations
  for (n in rulenames) {
        ind <- as.indicator(get(n))
        if(!missing(instrument))
          ind <- ind[J(Instrument=instrument)]
        DT <- DT[Date %between% range(ind$Date) ]
        DT[ind, Title:= paste(ifelse(is.na(Title),"", paste(Title, ",", sep=" "))
                              , n), mult="first"]
        DT[ind, Annotation:= paste(ifelse(is.na(Annotation)
                                          ,"", paste(Annotation, ",", sep=" ")) 
                                   , paste(n, ind$Signal, sep=" = ")), mult="first"]
  }
    
  if(plot) {
    library(googleVis)
    gv <- gvisAnnotatedTimeLine(DT, datevar="Date",
                                numvar="Value", idvar="Variable",
                                titlevar="Title", annotationvar="Annotation",
                                #                             options=list(displayAnnotations=FALSE,
                                #                                          legendPosition='newRow',
                                #                                          width=600, height=350)
                                options=list(displayAnnotations=TRUE,
                                             zoomStartTime=as.Date(max(DT$Date))-100,
                                             zoomEndTime=as.Date(max(DT$Date)),
                                             colors="['blue', 'lightblue']",
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
  View(DT, title="Check signals")
}


#' Check for new signals to act upon
#' 
#' @export
Monitor <- function() {
  
}


# 
# 
# # signal <- function(var, thres, op)  {
# #   structure(list(var=var, thres=thres, op=op), class="signal")
# # }
# signal <- function(call, data=NULL)  {
#   structure(list(call=call, data=data)
#             , class="signal")
# }
# 
# ls_signals <- function (envir=.GlobalEnv) {
#   all <- ls(envir=envir)
#   all[sapply(all, function(x) class(get(x))[1] == "signal")]
# }
# 
# #' @return \code{NULL}
# #'
# #' @rdname calc
# #' @method calc signal
# #' @S3method calc signal
# calc.signal <- function(x, name, ...) {
#   var <- calc(x$var)$data
#   thres <- eval(x$thres)
#   op <- x$op
#   if(is.numeric(thres) | is.logical(thres))
#     expr <- substitute(op(Value, thres))
#   sig.data <- var[,Signal:=eval(expr), by=Instrument]
#   sig.data[,Value:=NULL]
#   
#   if(!missing(name)) {
#     if(is.symbol(name)) name <- deparse(name)
#     setnames(sig.data, "Signal", name )
#   } 
#   x$data <- sig.data
#   return(x)
# }
# # calc.signal <- function(x, name, ...) {
# #   sig <- eval(x$call)
# #   if(!missing(name)) {
# #     if(is.symbol(name)) name <- deparse(name)
# #     setnames(sig$data, "Signal", name )
# #   }  
# #   return(sig)
# # }
# 
# #' @method dat signal
# #' @S3method dat signal
# dat.signal <- function(x, name) {
#   # TODO: not sure if this function is needed
#   calc.signal(x, name=name)$data
# }
# 
# #' @method print indicator
# #' @S3method print indicator
# print.signal <- function(x, ...) {
#   sig <- calc(x, with.name=FALSE)
#   print(sig$data)
# }
# 
# #' x is a data.table Uninformative values are removed: NAs and repetitive values. Only
# #' the leading signal is retained.
# sig.compress <- function(x) {
#   
#   has.changed <- function(x) {
#     ch <- c(TRUE,diff(x))
#     ch <- as.logical(ch)
#   }
#   
#   x <- x[x[!is.na(Signal),list(Instrument,Date)],] # remove NAs
#   x[has.changed(Signal), list(Date, Signal), by=Instrument]
#   x[, Signal:=ifelse(has.changed(Signal),Signal,NA), by= Instrument]
#   x <- x[x[!is.na(Signal),list(Instrument,Date)],]
#   return(x)
# }
# 
# #' @method Ops indicator
# #' @S3method Ops indicator
# `Ops.signal` <- function(x, y) {
#   op <- as.name(.Generic)
#   return(signal(call = substitute(op(x, y)), data=NULL))
# }
# 
# 
# `!.signal` <- function(x) {
#   op <- as.name(.Generic)
#   .call <- substitute(op(x))
#   
#   x <- eval(x$call)
#   .formula <- quote(!Signal)
#   data <- x$data[, Signal:=eval(.formula)]
#   
#   return(signal(call = .call, data=data))
# }
# 
# #' Some text
# #' 
# #' @rdname %AND%
# #' @export %AND%
# `%AND%` <- function(x, ...) {
#   UseMethod("%AND%",x)
# }
# 
# #' @method %AND% signal
# #' @S3method %AND% signal
# `%AND%.signal` <- function(x, y) {
#   # TODO: test if setkey is necessary after rbind
#   op <- as.name(.Generic)  
#   .call <- substitute(op(x, y))
#   #   X <- eval(x$call)$data # used to be sig.compress
#   #   Y <- eval(y$call)$data # used to be sig.compress
#   
#   X <- calc.signal(x, name="Signal.x")$data
#   Y <- calc.signal(y, name="Signal.y")$data
#   
#   Z <- unique(setkey(rbind(X[Y, roll = T, rollends=FALSE],
#                            Y[X, roll = T, rollends=FALSE],
#                            use.names = TRUE),
#                      Instrument, Date)
#   )[,Signal := as.logical(Signal.x * Signal.y)]
#   
#   Z <- Z[, list(Instrument, Date, Signal)]
#   setkey(Z, Instrument, Date) #  delete line after upgrading data.table beyond rev. 999
#   #   Z <- sig.compress(Z)
#   
#   return(signal(call = .call, data=Z))
# }
# 
# #' Some text
# #' 
# #' @rdname %OR%
# #' @export %OR%
# `%OR%` <- function(x, ...) {
#   UseMethod("%OR%",x)
# }
# 
# #' @method %OR% signal
# #' @S3method %OR% signal
# `%OR%.signal` <- function(x, y) {
#   # TODO: test if setkey is necessary after rbind
#   op <- as.name(.Generic)  
#   .call <- substitute(op(x, y))
#   #   X <- eval(x$call)$data # used to be sig.compress
#   #   Y <- eval(y$call)$data # used to be sig.compress
#   return(signal(call=.call))
#   X <- calc.signal(x, name="Signal.x")$data
#   Y <- calc.signal(y, name="Signal.y")$data
#   
#   Z <- unique(setkey(rbind(X[Y, roll = T, rollends=FALSE],
#                            Y[X, roll = T, rollends=FALSE],
#                            use.names = TRUE),
#                      Instrument, Date)
#   )[,Signal := as.logical(Signal.x + Signal.y)]
#   
#   Z <- Z[, list(Instrument, Date, Signal)]
#   setkey(Z, Instrument, Date) #  delete line after upgrading data.table beyond rev. 999
#   #   Z <- sig.compress(Z)
#   
#   return(signal(call = .call, data=Z))
# }
# `%OR%.signal` <- function(x, y) {
#   # TODO: test if setkey is necessary after rbind
#   op <- as.name(.Generic)  
#   .call <- substitute(op(x, y))
#   #   X <- eval(x$call)$data # used to be sig.compress
#   #   Y <- eval(y$call)$data # used to be sig.compress
#   
#   X <- calc.signal(x, name="Signal.x")$data
#   Y <- calc.signal(y, name="Signal.y")$data
#   
#   Z <- unique(setkey(rbind(X[Y, roll = T, rollends=FALSE],
#                            Y[X, roll = T, rollends=FALSE],
#                            use.names = TRUE),
#                      Instrument, Date)
#   )[,Signal := as.logical(Signal.x + Signal.y)]
#   
#   Z <- Z[, list(Instrument, Date, Signal)]
#   setkey(Z, Instrument, Date) #  delete line after upgrading data.table beyond rev. 999
#   #   Z <- sig.compress(Z)
#   
#   return(signal(call = .call, data=Z))
# }



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

