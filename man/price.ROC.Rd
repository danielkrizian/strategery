\name{price.ROC}
\alias{price.ROC}
\title{Calculate returns as rates of change between subsequent prices}
\usage{
  price.ROC(x, lag = 1, type = c("discrete", "continuous"),
    na.pad = TRUE, base.incl = TRUE)
}
\arguments{
  \item{x}{numeric vector or xts object representing prices
  TODO: test xts as input}
}
\description{
  Calculate returns as rates of change between subsequent
  prices
}

