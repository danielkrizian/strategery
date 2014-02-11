ls_rules <- function (envir=.GlobalEnv) {
  all <- ls(envir=envir)
  all[sapply(all, function(x) class(get(x))[1] == "rule")]
}

eval.rule <- function(rule) {
  if(is.character(rule))
    rule <- get(rule)
  sigdata <- calc(rule$signal)$data[Signal==TRUE,][,Signal:=NULL]
  if(is.numeric(rule$size))
    rule$portfolio <- sigdata[, Pos:=rule$size]
  return(rule)
}

print.rule <- function(rule) {
  print(eval.rule(rule)$portfolio)
}

#' @method dat rule
#' @S3method dat rule
dat.rule <- function(rule) {
  eval.rule(rule)$portfolio
}