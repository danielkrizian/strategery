#' Some Title
#' @export
createCluster = function
(
  noCores=detectCores()
  ,parVar = NULL
  ,packages = NULL
  ,logfile = "/dev/null"
) {
  require(parallel)
  require(doParallel)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(parVar)) clusterExport(cl, parVar)
  if(!is.null(packages)) {
    l_ply(packages, function(dum) {
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoParallel(cl)
  return(cl)
}

# cl <- createCluster(detectCores(), parVar = as.list(ls()), packages = as.list((.packages())))