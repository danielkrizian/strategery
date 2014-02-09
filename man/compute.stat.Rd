\name{compute.stat}
\alias{compute.stat}
\title{Compute performance statistic}
\usage{
  compute.stat(stat, ..., name = NULL)
}
\description{
  computes statistic, using function or name of the
  statistic supplied. TODO (Function intelligently looks up
  required arguments from the object slots. Some arguments
  are assumed, even if not provided with exact name: e.g.
  if slot 'ret' exists, it will be passed to the functions
  as 'x' argument) stats - character vector of either
  function names or names of statistics (as defined) ... -
  arguments passed to functions names - (optional) names to
  apply to returned vector returns named numeric vector of
  statistics OPTIONAL TODO: common argument conversions: if
  supplied, convert 'ret' argument name to 'x'
  if(all(pm==0)) # desired argument has not been supplied,
  so perform common conversions where applicable
  names(args)[pmatch(names(args), 'R', nomatch = 0L) > 0L]
  <- 'x' #rename arg R to x pm <- pmatch(names(args),
  onames, nomatch = 0L)
}

