\name{Prepare.market}
\alias{Prepare.market}
\title{Some title}
\usage{
  Prepare.market(symbols, lookup,
    align = c("keep.all", "remove.na"), dates = NULL,
    fill.gaps = F)
}
\description{
  goal: market$prices must contain trading opportunities of
  the relevant model. Columns are symbols. Make OpenClose
  frequency if necessary. remove NAs for speed. Retain NAs
  for accuracy. Info stored in has.NAs TODO: support
  OpenClose frequency
}

