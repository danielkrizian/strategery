\name{summary.returns}
\alias{summary.returns}
\title{Calculate summary statistics for returns}
\usage{
  summary.returns(x,
    byIns = ifelse(is.null(weights), T, F), byPer = F,
    weights = NULL)
}
\arguments{
  \item{byIns}{a logical value. Calculate for each
  Instrument separately?}

  \item{byPer}{Either logical (TRUE is equivalent to
  "years") or character, one of c("months","years")}

  \item{weights}{a numeric vector or data.table of
  portfolio weights. If provided, portfolio returns are
  calculated.}
}
\description{
  Calculate summary statistics for returns
}

