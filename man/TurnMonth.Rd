\name{TurnMonth}
\alias{TurnMonth}
\title{Days around the end and the beginning of the month}
\usage{
  TurnMonth(first.n.bds, last.n.bds, lag = 0)
}
\arguments{
  \item{lag}{integer, indicating whether to lag the date
  (positive integer), or advance the date (negative
  integer) by (lag) number of business days Special case:
  (0,0) is converted to (0,1)}
}
\description{
  eomNYSE is stored as index of monthly endpoints of bdNYSE
  eomNYSE <- which(diff(as.numeric(format(bdNYSE, format =
  "%m")))!=0)
}

