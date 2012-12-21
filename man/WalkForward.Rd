\name{WalkForward}
\alias{WalkForward}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WalkForward(strategy, train.years = 5, test.years = 1, fitFUN = compute.edge, dates = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strategy}{
%%     ~~Describe \code{strategy} here~~
}
  \item{train.years}{
%%     ~~Describe \code{train.years} here~~
}
  \item{test.years}{
%%     ~~Describe \code{test.years} here~~
}
  \item{fitFUN}{
%%     ~~Describe \code{fitFUN} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
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
function (strategy, train.years = 5, test.years = 1, fitFUN = compute.edge, 
    dates = NULL, ...) 
{
    require(lubridate)
    if (is.character(strategy)) 
        strategy <- getStrategy(strategy)
    data <- if (length(dates)) 
        market$prices[dates]
    else market$prices
    test.periods <- split(data, f = "years", k = test.years)
    test.periods[1:(train.years + 1)] <- NULL
    folds <- unlist(lapply(test.periods, function(x) paste(range(index(x)), 
        collapse = "::")))
    P <- portfolio(paste("wf", strategy$name, sep = "-"), store = F)
    for (fold in folds) {
        test.start <- as.Date(unlist(strsplit(fold, split = "::"))[1])
        test.end <- as.Date(unlist(strsplit(fold, split = "::"))[2])
        train <- paste(test.start - years(train.years), test.start - 
            days(1), sep = "::")
        train <- Train(strategy, fitFUN = fitFUN, dates = train, 
            ... = ...)
        message("Testing on fold: ", fold)
        P <- Test(strategy, train$pars, dates = fold, portfolio = P, 
            details = F)
        B <- Benchmark(type = "Random", portfolio = P)
        R <- Returns(P, B)[fold]
        stat.test <- compute.premium(R[, 1], R[, 2])
        message("Performance: ", stat.test)
        fit.pars <- xts(t(train$pars), order.by = test.end)
        stat.train <- xts(train$opt$bestval, order.by = test.end)
        stat.test <- xts(stat.test, order.by = test.end)
        P$fit.pars <- c(fit.pars, P$fit.pars)
        P$stat.train <- c(stat.train, P$stat.train)
        P$stat.test <- c(stat.test, P$stat.test)
    }
    return(P)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
