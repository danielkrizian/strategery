##### src/id/var/date/value ######

getData <- function(pattern, src, ids, fields, 
                    time, 
                    FUN, online=F, ...) {
  args = match.call()
  if(!online) {
    FUN = "datagery"
    args = list(ids=datageryIds(lookup=args$ids, src=args$src),
                fields = args$fields,
                start_date = args$time[1],
                end_date = args$time[2])
  }
  
  do.call(FUN, args=args)
}

datageryIds <- function(lookup, src) {
  if(!exists("INSTRUMENT", .GlobalEnv))
    loadDB("INSTRUMENT")
  if(src=="Bloomberg") {
    srcnm = as.name(src)
    lookup = eval(lookup)
    filter = substitute(srcnm %in% lookup)
    INSTRUMENT[eval(filter)][[src]]
  }
}


datagery <- function(ids, fields=c("prices","returns"), time) {
  src.prefix = "datagery"
  args = as.list(match.call()[-1])
  flds= match.arg(fields)
  FUN = paste(src.prefix, flds, sep = ".")
  args$fields <- NULL
  do.call(FUN, args)
}

datagery.prices <- function(ids, time) {
  if(!exists("PX", .GlobalEnv))
    loadDB("PX")
  out = PX[J(Ticker=ids)]
  setnames(out, "Value", "Price")
  return(out)
}

datagery.returns <- function(ids, time) {
  if(!exists("R", .GlobalEnv))
    loadDB("R")
  out = R[FundName %like% ids]
  setnames(out, "Value", "Return")
  return(out)
}


#get object satisfying criteria
datagery.objects <- function(criteria=list(class="fund", 
                                           connects.to="",
                                           has.edge="")) {
  if(identical(criteria)
}

edhec <- function(ids, fields="returns", time, ...) {
  src.prefix = "edhec"
  args = as.list(match.call()[-1])
  flds= match.arg(fields)
  FUN = paste(src.prefix, flds, sep = ".")
  args$fields <- NULL
  do.call(FUN, args)
}

edhec.returns <- function(ids, time, username, pwd) {
  require(RCurl)
  loginurl = "http://www.edhec-risk.com/login_form"
  dataurl  = "http://www.edhec-risk.com/indexes/pure_style/data/table/history.csv"
  pars=list(
    "__ac_name"=username,
    "__ac_password"=pwd
  )
  agent="Mozilla/5.0" #or whatever 
  curl = getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl)
  html=postForm(loginurl, .params = pars, curl=curl)
  html=getURLContent(dataurl, curl=curl)
  
  rm(curl)
  
  out = read.csv(textConnection(html), sep=";", check.names=F)
  out[, -1] = apply(out[, -1], 2, 
                    FUN = function(x) as.numeric(gsub("%", "", x, fixed=TRUE)))
  out[, 1] = as.Date(out[, 1], format = "%d/%M/%Y")
  out = data.table:::melt.data.table(as.data.table(out), 
                                     id.vars = "date", variable.name="ID",
                                     value.name="Return")
  out[, Return:=Return/100]
  setnames(out, "date", "Date")
  setcolorder(out, c("ID", "Date", "Return"))
  setkey(out, ID, Date)
  if(!missing(ids))
    out = out[J(ID=ids)]
  
  return(out)
}

#' Read rds files, assign to the environment.
#' @export
loadDB <- function(tables=c("INSTRUMENT","OHLCV"), path=getOption("DBpath"), envir=.GlobalEnv) {
  for(table.name in tables) {
    file=file.path(path, paste(table.name, ".rds", sep=""))
    out <- readRDS(file)
    assign(table.name,out, envir=envir)
  }
}

#' getOHLCV
#' 
#' @param x An xts object with OHLC-like structure (quantmod::is.OHLC(x) == TRUE) 
#' @export
getOHLCV.yahoo <- function(ids, from="1900-01-01"){
  require(quantmod)
  
  col.names <- c("Instrument","Date","Open", "High", "Low","Close", "Volume","Adjusted" )
  
  new <- NULL
  for(id in ids) {
    D <- as.data.table(getSymbols(id, src="yahoo", from=from, auto.assign=FALSE))
    setnames(D, names(D), sub(paste(id,".",sep=""),"", names(D)))
    D[,Instrument:=id]
    setcolorder(D, col.names)
    new <- rbindlist(list(new,D))
  }
  setkey(new, Instrument, Date)
  
  return(new)
}

getOHLCV.Bloomberg <-  function(ids, from="1900-01-01") {
  require(Rbbg)
  conn <- blpConnect()
  
  start.date <- format(as.Date(from),"%Y%m%d")
  end.date <- NULL
  fields <- c("PX_LAST")
    
  toimport <- bdh(conn, ids, fields, start.date, end.date
                  , option_names=c(  "nonTradingDayFillOption"
                                     , "nonTradingDayFillMethod")
                  , option_values=c(  "ACTIVE_DAYS_ONLY"
                                      , "NIL_VALUE"))
  toimport <- as.data.table(toimport)
  setnames(toimport, old=colnames(toimport)
           , new=c("Ticker", "Date", "PX_Last"))
  
}

getData.Bloomberg <- function(ids, fields=c("PX_LAST"), from="1930-01-01") {
  # TODO(dk): support for fields vector
  require(Rbbg)
  conn <- blpConnect()
  if("PX_LAST" %in% fields || "YLD_YTM_MID" %in% fields) {
    # working ranges as of 20140424:
    # C0A0 Index: 19231112,20140406
    # G0Q0 Index: 19231130,20140424
    start.date <- format(as.Date(from),"%Y%m%d")
    end.date <- format(Sys.Date()-20,"%Y%m%d")
    .data <- bdh(conn, ids, fields, start.date, end.date
                 , option_names=c(  "nonTradingDayFillOption"
                                    , "nonTradingDayFillMethod")
                 , option_values=c(  "ACTIVE_DAYS_ONLY"
                                     , "NIL_VALUE"))
    .data <- as.data.table(.data)
    if(length(ids)==1) .data[,ticker:=ids]
    setnames(.data, c("ticker","date",fields),c("Ticker","Date","Value"))
    setcolorder(.data,c("Ticker","Date","Value"))
    .data[,Date:=as.IDate(Date)]
    .data[,Value:=as.numeric(Value)]
    if(identical(fields,"PX_LAST") || identical(fields,"YLD_YTM_MID"))
      return(.data)
#   } else {
#     bdp(conn, c("G0O1 Index","G0O2 Index"), "MLI_TOT_RTN_LOC", "MLI_DATE","20140106")
#     l <-lapply(as.list(c("20140103","20140106")),
#            function(x) {
#              downloaded <- bdp(conn, c("G0O1 Index","G0O2 Index"), "MLI_TOT_RTN_LOC", "MLI_DATE",x)
#              downloaded$Ticker <- row.names(downloaded)
#              downloaded$Date <- as.Date(x, format="%Y%m%d")
#              downloaded
#            })
  
  } else { 
    # this branch probably not needed (supposed to solve MLI indices)
    .data <- bdp(conn, securities=ids,fields=fields)
  }
  .data <- as.data.table(.data)
  if("date" %in% names(.data)) {
    .data$date <- as.IDate(.data$date)
    setnames(.data, "date", "Date")
  }
  return(.data)
}

getOHLCV.rds <-  function(ids, from="1900-01-01") {
    loadDB(tables="OHLCV")
}

getInstrument.odbc <- function(ids, idfield,
                               sql="SELECT * FROM Instrument WHERE @idfield IN @ids", 
                               dsn=NULL, uid="", pwd="") {
  
  require(RODBC); require(data.table)
  if(missing(ids)) {
    sql <- sub(" WHERE @idfield IN @ids", "", sql)
  }
  else {
    sql <- sub("@idfield", "Bloomberg", sql)
    sql <- sub("@ids", paste0("('",paste0(ids, collapse="','"),"')"), sql)
  }
  channel <-odbcConnect(dsn, uid, pwd)
  as.data.table(sqlQuery(channel, sql))
}

search.data.table <- function(x,pattern, whole=FALSE, ignore.case=T, perl=FALSE, fixed=FALSE, which=FALSE) {
  if(!is.data.table(x))
    x <- as.data.table(x)
  nms <- names(x)
  
  if(whole){
    anycol <- paste0(nms, "=='", pattern, "'")
  } else {
    paramstr = sprintf("ignore.case=%s, perl=%s, fixed=%s", ignore.case, perl, fixed)
    anycol <- paste0("grepl('", pattern, "', ", nms, ",", paramstr , ")") 
  }
  allcol <- paste0(anycol, collapse = " | ")
  allcol <- eval(expression(allcol))
  if(!which)
    return(x[eval(as.call(parse(text=allcol))[[1]])])
  else {
    rows = x[eval(as.call(parse(text=allcol))[[1]]), which = TRUE]
    cols = pmatch(names(x)[unlist(lapply(x, function(col) (pattern %in% col)))], names(x))
    return(data.table(rows=rows, cols=cols))
  }
}

#' this is old search instrument function dependent on FinancialInstrument package
Search <- function(pattern, table="INSTRUMENT", fields="all") {
  if(!is.data.table(table))
  table <- get(table)
  out <- search.data.table(table, pattern)
  View(out)
  return(out)
}

