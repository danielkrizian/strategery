# benchmarking, debugging

####### PERFORMANCE ###########
#' Some Title
#' @export
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  #   Is there an R timer or stopwatch function similar to MATLAB's tic/toc?
  #   http://stackoverflow.com/questions/1716012/stopwatch-function-in-r/1716344#1716344
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

#' Some Title
#' @export
toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

########## DEBUGGING ############

#' Some Title
#' 
#' read R Inferno Circle 8
#' @export
my.debug <- function() {
  options(warn=2) ### options(warn=1)
  options(error=recover) # options(error=stop)
}
# sys.call()
#' Some Title
#' 
#' @export
my.undebug <- function() {
  options(warn=1)
  options(error=stop)
}

#' Some Title
#' @export
my.perf <- function(expr) {
  # my.perf(quote(example(glm)))
  Rprof(tmp <- tempfile())
  eval(expr)
  Rprof()
  ret <- summaryRprof(tmp)
  unlink(tmp)
  return(ret)
}

