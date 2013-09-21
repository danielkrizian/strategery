\name{Fill}
\alias{Fill}
\title{Remove excessive signals}
\usage{
  Fill(x, y = !x)
}
\description{
  works as a flip/flop device or "latch"
  (electronic/electric engineers will know what I mean
  returns 1 from the first occurence of TRUE signal in x
  until a TRUE occurs in y which resets the state back to
  zero unil next TRUE is detected in x... this essentially
  reverts the process of ExRem - multiple signals are back
  again TEST : fill(c(1,1,0,1),c(1,0,0,0))
}

