options(lazy.indicators=F)
options(param.indicators=F)

eval_params = function(expr, envir = parent.frame(), enclos = parent.frame()) {
  lapply(expr, function(m) {
    if (is.call(m)) {
      if (m[[1]] == quote(eval)) eval(m[[2]], envir, enclos)
      else eval_params(m, envir, enclos)
    } else if(is.name(m)){
      if(exists(as.character(m),envir=as.environment(.GlobalEnv)) & !existsFunction(as.character(m))) {
        obj <- eval(m, envir=as.environment(.GlobalEnv))
        if(mode(obj) %in% c("numeric","character" ))
          obj
        else m
      } else m
    } else m
  })
}

#' Construct indicator
#' 
#' @rdname indicator
#' @export indicator
indicator <- function(data, trans, lazy=F) {
#   if(is.sfl(substitute(trans)) & missing(data))
#     return(eval(trans))
  if(missing(lazy) & !is.null(getOption("lazy.indicators")))
    lazy=getOption("lazy.indicators")
  if(!lazy) {
    strans <- substitute(trans)
    i = Indicator$new(data=data, trans=strans)
    return(i$eval())
  }
  op <- as.name("%indicator%")
  sfl.expr <- substitute(op(data, trans))
  return(structure(.Data=sfl.expr, class=c("sfl")))
}

#' @rdname %indicator%
#' @export %indicator%
`%indicator%` <- function(e1, e2) {
  t = substitute(e2)
  i = Indicator(data=e1, trans=t)
  i$eval()
  return(i)
}

Indicator.initialize <- function(..., data=data.table(), 
                                 trans=quote(`<undef>`()), 
                                 col=character(),
                                 id.col=character(),
                                 time.col=character(),
                                 evaled=logical()) {
  callSuper(...)
  
  if(!length(data))
    return(.self)
  
  timeBased = function(x) {
    if (!any(sapply(c("Date", "POSIXt", "chron", 
                      "dates", "times", 
                      "timeDate", "yearmon", 
                      "yearqtr", "xtime"), function(xx) inherits(x, xx)))) {
      FALSE
    } else TRUE
  }
  
  # validating/setting key
  k = key(data)
  if(length(k) < 2) {
    warning("No/incomplete key found in the data.") 
    nm = names(data)
    tb = sapply(data, timeBased)
    if(!length(tb))
      stop("Indicator data must contain timeBased column.")
    
    if(length(k)==1) {
      if(any(k %in% tb)) {
        id.col = ifelse(length(id.col), id.col, nm[1])
        time.col = ifelse(length(time.col), time.col, tail(k %in% tb, 1))
      } else {
        id.col = ifelse(length(id.col), id.col, k)
        time.col = ifelse(length(time.col), time.col, names(tb)[tb][1])
      }
      warning("Only one-column key, (", k,  "), found in the data. If you intended to create an
         univariate indicator, create new id column containing series name and 
         include it in the key, together with time column. Setting keys automagically now:")
    }
    if(length(k)==0) {
      id.col = ifelse(length(id.col), id.col, names(tb)[!tb][1])
      time.col = ifelse(length(time.col), time.col, names(tb)[tb][1])
    }
    setkeyv(data, c(id.col, time.col))
    warning("Set (", key(data), ") as key")
  }

  if(missing(col) & length(key(data)) & length(names(data))) {
    col=setdiff(names(data),key(data))
  }

  .self$trans <- trans
  .self$data <- data
  if(length(id.col) > 1)   # TODO: Multi-column indicator ids
    stop("Multi-column ids not supported yet")
  .self$.id <- ifelse(length(id.col), id.col, key(data)[-length(key(data))] )
  .self$.time <- ifelse(length(time.col), time.col, key(data)[length(key(data))])
  if(length(trans)==1 & 
       !identical(as.character(trans),as.character(quote(`<undef>`()))))
    .self$.col <- as.character(trans)
  else
    .self$.col <- col
  
  if(missing(evaled))
    .self$evaled <- FALSE
  return(.self)
}

is.evaled <- function(x) {
  x$evaled
}

#' @method Ops Indicator
#' @S3method Ops Indicator
Ops.Indicator <- function(e1, e2) {
  op <- as.name(.Generic)
  if(is.numeric(e2) | is.logical(e2)) {
#     expr <- substitute(op(Value, e2))
#     if(as.character(op) %in% c("==", "!=", "<", "<=", ">=", ">")) {
#       cl <- class(e1$Value)
#       .data <- e1[,Value:=as(eval(expr), cl), by=Instrument]
#       .data[,Value:=as.logical(Value)]
#       # because assigning directly Value:=eval(expr) throws error: 
#       # "Type of RHS ('logical') must match LHS."
#     }
    e1$eval()
    e1trans <- e1$trans
    trans <- substitute(op(e1trans, e2))
    out <- Indicator(data=e1$data, trans=trans)
    out$eval()
    return(out)
  }
  else if(inherits(e2,"Indicator")) {
    if(address(e1$data)==address(e2$data)) {
      e1$eval()
      e2$eval()
      e1trans <- e1$trans
      e2trans <- e2$trans
      trans <- substitute(op(e1trans, e2trans))
      out <- Indicator(data=e1$data, trans=trans)
      out$eval()
      return(out)
    }
    
    if(as.character(op) %in% c("&", "|")) {
      .data <- unique(setkey(rbind(e1[e2, roll = T, rollends=FALSE],
                                   e2[e1, roll = T, rollends=FALSE],
                                   use.names = TRUE),
                             Instrument, Date))
      if(as.character(op) =="&")
        .data[,Value := as.logical(Value * Value.1)]
      else
        .data[,Value := as.logical(Value + Value.1)]
      
      .data <- .data[, list(Instrument, Date, Value)]
    } else {
      if(address(e1$data)==address(e2$data)) {
        e1$eval()
        e2$eval()
        Indicator(e1$data, trans=substitute(op(e1$trans, e2$trans)))
      } else {
        stop("build address(e1$data)==address(e2$data)")
      }
      .data <- unique(setkey(rbind(e1[e2, roll = T, rollends=FALSE],
                                   e2[e1, roll = T, rollends=FALSE],
                                   use.names = TRUE),
                             Instrument, Date))
      expr <- substitute(op(Value, Value.1))
      .data[,Result := eval(expr)]
      .data <- .data[, list(Instrument, Date, Result)]
      setnames(.data, "Result", "Value")
    }
  }
  return()
}

#' @method %AND% Indicator
#' @S3method %AND% Indicator
`%AND%.Indicator` <- function(e1, e2) {
  # TODO: merge into `Ops.Indicator` as (e1 & e2)
  op <- as.name("&")
  e1$eval()
  e2$eval()
  e1trans <- e1$trans
  e2trans <- e2$trans
  trans <- substitute(op(e1trans, e2trans))
  
  if(address(e1$data)==address(e2$data)) {
    out <- Indicator(data=e1$data, trans=trans)
    out$eval()
    return(out)
  } else {
    fullouter <- unique(setkey(rbind(e1$data[e2$data, roll = T, rollends=FALSE],
                                  e2$data[e1$data, roll = T, rollends=FALSE],
                                  use.names = TRUE),
                            Instrument, Date))
    # TODO: remove "Instrument, Date" name dependency
    # see also this for single-columned key:
    # http://stackoverflow.com/questions/12773822/why-does-xy-join-of-data-tables-not-allow-a-full-outer-join-or-a-left-join
    .col = paste0(e1$.col," & ", e2$.col)
    fullouter[, c(.col):= get(e1$.col) & get(e2$.col)]
    out <- Indicator(data=fullouter, trans=trans, col=.col)
    return(out)
  }
}

#' @method %OR% Indicator
#' @S3method %OR% Indicator
`%OR%.Indicator` <- function(e1, e2) {
  # TODO: merge into `Ops.Indicator` as (e1 | e2)
  op <- as.name("|")
  e1$eval()
  e2$eval()
  e1trans <- e1$trans
  e2trans <- e2$trans
  trans <- substitute(op(e1trans, e2trans))
  if(address(e1$data)==address(e2$data)) {
    out <- Indicator(data=e1$data, trans=trans)
    out$eval()
    return(out)
  } else {
    fullouter <- unique(setkey(rbind(e1$data[e2$data, roll = T, rollends=FALSE],
                                     e2$data[e1$data, roll = T, rollends=FALSE],
                                     use.names = TRUE),
                               Instrument, Date))
    # TODO: remove "Instrument, Date" name dependency
    # see also this for single-columned key:
    # http://stackoverflow.com/questions/12773822/why-does-xy-join-of-data-tables-not-allow-a-full-outer-join-or-a-left-join
    .col = paste0(e1$.col," | ", e2$.col)
    fullouter[, c(.col):= get(e1$.col) | get(e2$.col)]
    out <- Indicator(data=fullouter, trans=trans, col=.col)
    return(out)
  }
}


Indicator.chart <- function(){
  chartdata <- .self$wide()
  dy <- dygraph(data=chartdata, sync=TRUE, crosshair="vertical", legendFollow=TRUE, width=1000)
  dy$show(cdn=TRUE)
}

Indicator.countIds <- function(){
  length(unique(data[, .id, with=FALSE][[.id]]))
}

#' Three types: 1.untransformed column, 2. function based on data 3. function based on other indicator(s)  
Indicator.eval = function(){
  if(length(trans)>1) {
    label = if(getOption("param.indicators"))
      deparse(construct(eval_params(trans)), width.cutoff=500)
    else
      deparse(trans, width.cutoff=500)
    .col <<- label
    data[, `:=`(substitute(label), eval(trans)), by=.id]
  }
  evaled <<- TRUE
  return(.self)
}

Indicator.print = function() {
  .self$eval()
  data.table:::print.data.table(data[,c(.id, .time, .col), with=FALSE])
}

Indicator.range = function() {
  range.default(data[, .col, with=FALSE], na.rm=TRUE)
}

# returns xts of the raw data, with columns as ids and rows as time. 
Indicator.xts <- function(col=.col) {
  DT = wide(col)
  row.names = DT[, .time, with=F][[.time]]
  DT[, c(.time):=NULL]
  out = xts::as.xts(as.data.frame(DT), order.by=as.Date(as.character(row.names))) # as.Date because unsupported 'indexClass' indexing type: IDate
  return(out)
}

Indicator.resample <- function(rule, how="last"){
  # grepl(" month | months | month| months|month |months |month|months",rule, ignore.case=TRUE)
  # grepl(" hour | hours | hour| hours|hour |hours |hour|hours",rule, ignore.case=TRUE)
  # grepl(" min | minutes | min| minutes|min |minutes |min|minutes",rule, ignore.case=TRUE)
  # grepl(" sec| secs| seconds|sec|secs|seconds",rule, ignore.case=TRUE)
  require(xts)
  require(lubridate)
  if(grepl(" quarter | quarters | quarter| quarters|quarter |quarters |quarter|quarters"
           ,rule, ignore.case=TRUE)) {
    if(how=="last")
    data <<- data[,.SD[endpoints(eval(as.name(.time)), on = "quarters")], by=.id]
    freq <<- 4L
  }
  if(grepl(" month | months | month| months|month |months |month|months"
           ,rule, ignore.case=TRUE)) {
    if(how=="last") {
      data <<- data[,.SD[endpoints(eval(as.name(.time)), on = "months")], by=.id]
      # find last day of a current month
      data[,c(.time):=floor_date(eval(as.name(.time)),"month")+months(1)-days(1)] # e.g. 2013-11-29 (last BD) to 2013-11-30
      freq <<- 12L
    }
  }
  setkeyv(data, c(.id, .time))
  return(.self)
}

Indicator.subset <- function(id=NULL, time=NULL){
  .self$eval()
  data <<- .self[time, id]$data
  return(.self)
}

Indicator.wide <- function(col=.col){
  if(!length(col))
    .self$eval()
  if(is.univariate(.self))
    return(data[, c(.time, col), with=FALSE])
  else
    return(dcast.data.table(data, formula= as.formula(paste0(.time, " ~ ", .id)), value.var = col))
}

is.indicator <- function(x) {
  inherits(x,"Indicator")
}

is.signal <- function(x) {
  if(!inherits(x, "Indicator")) 
    return(FALSE)
  
  
}

ls_indicators <- function (envir=.GlobalEnv) {
  all <- ls(envir=envir)
  all[sapply(all, function(x) {
    is.indicator(eval(get(x)))}
  )]
}

is.discrete <- function(x, thres=0.05) {
  if(is.indicator(x))
    return(is.discrete(x$rawdata(), thres=thres))
  if(is.logical(x))
    return(TRUE)
  return(length(unique(x))/length(x) <= thres)
}

is.univariate <- function(x) {
  if(!is.indicator(x)) 
    stop(deparse(substitute(x))," must be of Indicator class. Supplied ", class(x))
  x$countIds() == 1
}

# x = c(1,100)
# y= c(2,105)
# z = c(0, 90)
# similar.scale(list(x,y))
similar.scale <- function(..., thres=0.7) {
  overlap_rate <- function(...) {
    outer = max(..., na.rm=T) - min(..., na.rm=T)
    inner = pmin(..., na.rm=T)[2] - pmax(..., na.rm=T)[1]
    inner / outer
  }
  l = list(...)
  if(length(l)==1)
    l = l[[1]]
  l = lapply(l, function(x) c(min(x, na.rm=T), max(x, na.rm=T)))
  r = do.call("overlap_rate", l)
  r >= min(max(thres, 0), 1)
}

#' @import data.table
Indicator = setRefClass('Indicator',
                        fields= list(data="data.table", trans="language", .id="character",
                                     .time="character", .col="character", evaled="logical",
                                     freq="integer"),
                        methods = list(initialize=Indicator.initialize,
                                       countIds=Indicator.countIds,
                                       chart=Indicator.chart,
                                       eval=Indicator.eval,
                                       print=Indicator.print,
                                       range=Indicator.range,
                                       resample=Indicator.resample,
                                       show=Indicator.print,
                                       subset=Indicator.subset,
                                       wide=Indicator.wide,
                                       xts=Indicator.xts)
)

##### Indicator[i, j] #####
#' @import zoo
#' @import xts
setMethod("[","Indicator", function(x, i, j, exact=FALSE, ..., drop=TRUE) {
  if(missing(i) & missing(j))
    return(x)
  stimefilter = NULL
  sidfilter = NULL
  if(!missing(i)){
    if(is.character(i)) {
      parsed <- xts::.parseISO8601(i)
      start <- as.Date(parsed$first.time)
      end <- as.Date(parsed$last.time)
      and <- if(any(is.na(c(start,end)))) "" else "&"
      
      startexpr <- if(is.na(start)) "" else paste0(x$.time, " >= '",start,"'")
      endexpr <- if(is.na(end)) "" else paste0(x$.time, " <= '",end,"'")
      stimefilter <- paste0(startexpr, and, endexpr)
    } else if(inherits(i,"Date")) {
      stop("Date subset yet to be supported. Prepared but not tested")
      # y$data <- y$data[eval(between(deparse(substitute(y$.time), i)))]
    } else stop("Non-character or non-Date i subsets not supported yet.")
  }
  if(!missing(j)) {
    if(length(x$.id)>1) stop("Multi-column ids not supported yet")
    if(exact) {
      sfilter <- paste0(x$.id, " == '", j ,"'", collapse=" | ")
    } else {
      sfilter <- paste0(x$.id, " %like% '", j ,"'", collapse=" | ")
    }
  }  
  stotalfilter <- paste0(c(sfilter, stimefilter), collapse=") & (")
  out <- x$copy(shallow=TRUE)
  out$data <- out$data[eval(parse(text=paste("(",stotalfilter, ")")))]
  return(out)
})
