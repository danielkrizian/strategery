\name{ExRem}
\alias{ExRem}
\title{Remove excessive signals}
\usage{
  ExRem(x, y = !x)
}
\description{
  returns 1 on the first occurence of "true" signal in x
  then returns 0 until y is true even if there are "true"
  signals in x
}

