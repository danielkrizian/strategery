\name{Test}
\alias{Test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Test(strategy, pars, dates = NULL, size = 1, portfolio = NULL, details = F, returns = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strategy}{
%%     ~~Describe \code{strategy} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{portfolio}{
%%     ~~Describe \code{portfolio} here~~
}
  \item{details}{
%%     ~~Describe \code{details} here~~
}
  \item{returns}{
%%     ~~Describe \code{returns} here~~
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
function (strategy, pars, dates = NULL, size = 1, portfolio = NULL, 
    details = F, returns = T) 
{
    if (is.character(strategy)) 
        strategy <- getStrategy(strategy)
    sig <- Signals(strategy, pars = pars)
    if (is.vector(size) & length(size) == 1) 
        size <- size
    else stop("Size argument in unrecognized format. TODO sizing.")
    oPfolio <- Execute(sig = sig, size, portfolio = portfolio, 
        dates = dates, details = details)
    if (returns) 
        oPfolio$R <- Returns(oPfolio, type = "periods", reduce = F, 
            refresh = T)
    return(oPfolio)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
