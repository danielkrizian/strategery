\name{Train}
\alias{Train}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Train(strategy, dates = NULL, fitFUN = compute.edge, control = list(strategy = 2, VTR = -4, NP = 30, itermax = 10, CR = 0.7, F = 0.8, parallelType = 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strategy}{
%%     ~~Describe \code{strategy} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
}
  \item{fitFUN}{
%%     ~~Describe \code{fitFUN} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (strategy, dates = NULL, fitFUN = compute.edge, control = list(strategy = 2, 
    VTR = -4, NP = 30, itermax = 10, CR = 0.7, F = 0.8, parallelType = 2)) 
{
    require(DEoptim)
    require(iterators)
    if (is.character(strategy)) 
        strategy <- getStrategy(strategy)
    f <- function(pars, strategy, dates = NULL) {
        ans <- compute.edge(strategy, pars, dates = dates)
        return(ans)
    }
    outDEoptim <- DEoptim(f, getParBounds(strategy)$lower, getParBounds(strategy)$upper, 
        control, strategy = strategy, dates = dates, fnMap = function(pars) round(pars))
    ret <- list()
    ret$pars <- outDEoptim$optim$bestmem
    ret$opt <- outDEoptim$optim
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
