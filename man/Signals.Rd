\name{Signals}
\alias{Signals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Signals(strategy, pars, granular = F, sim.gaps = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strategy}{
%%     ~~Describe \code{strategy} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{granular}{
%%     ~~Describe \code{granular} here~~
}
  \item{sim.gaps}{
%%     ~~Describe \code{sim.gaps} here~~
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
function (strategy, pars, granular = F, sim.gaps = T) 
{
    if (is.character(strategy)) 
        strategy <- getStrategy(strategy)
    sigFUN <- strategy$signals[["signal"]]$name
    sig <- do.call(sigFUN, as.list(pars))
    if (granular) {
        sig <- Align(sig, to = market$prices, pad = na.locf)
    }
    if (sim.gaps & market$has.NAs) {
        sig <- sig * market$prices/market$prices
        sig <- na.locf(sig, na.rm = F)
    }
    return(sig)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
