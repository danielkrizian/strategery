\name{Benchmark}
\alias{Benchmark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Benchmark(type = c("Hold", "Random", "rand pctile"), strategy, pars, dates = NULL, details = F, portfolio = Test(strategy, pars, dates = dates, details = details))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{strategy}{
%%     ~~Describe \code{strategy} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
}
  \item{details}{
%%     ~~Describe \code{details} here~~
}
  \item{portfolio}{
%%     ~~Describe \code{portfolio} here~~
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
function (type = c("Hold", "Random", "rand pctile"), strategy, 
    pars, dates = NULL, details = F, portfolio = Test(strategy, 
        pars, dates = dates, details = details)) 
{
    type = type[1]
    pos <- portfolio$pos
    if (type == "Hold") {
        pos[] <- 1
    }
    else if (type == "Random") {
        t <- NROW(pos)
        long.bias <- sum(pos > 0)/t
        short.bias <- sum(pos < 0)/t
        pos[] <- 1 * (long.bias - short.bias)
    }
    else stop("other than hold benchmarks not supported yet.")
    b <- portfolio(name = type, positions = pos, trades = F, 
        store = F)
    return(b)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
