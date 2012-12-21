\name{Execute}
\alias{Execute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Execute(sig, size = 1, portfolio = NULL, dates = NULL, do.lag = T, details = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{portfolio}{
%%     ~~Describe \code{portfolio} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
}
  \item{do.lag}{
%%     ~~Describe \code{do.lag} here~~
}
  \item{details}{
%%     ~~Describe \code{details} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (sig, size = 1, portfolio = NULL, dates = NULL, do.lag = T, 
    details = F, ...) 
{
    sig <- sig * size
    if (do.lag) {
        pos <- lag(sig, do.lag, na.pad = T)
        pos[1:do.lag, ] <- 0
    }
    else pos <- sig
    if (length(dates)) 
        pos <- pos[dates]
    if (is.null(portfolio)) 
        portfolio <- portfolio(store = F)
    portfolio <- updatePfolio(portfolio, positions = pos, trades = details, 
        store = F)
    return(portfolio)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
