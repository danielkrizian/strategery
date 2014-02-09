\name{to.interval.returns}
\alias{to.interval.returns}
\title{#Constructor for class 'returns' - time series of returns
#
#Convert object to a class time series of returns
#
#}
\usage{
  to.interval.returns(x, to,
    fill = if (to == "weeks") c(1, 4) else if (to == "months") c(3, 3) else FALSE,
    eow.fri = T)
}
\arguments{
  \item{x}{data.table. TODO: xts #}

  \item{interval}{character vector as one of c("monthly",
  "regular","MTD", NA, ...). #Sets the interval property of
  returns object. If missing, the function will attempt #to
  discern the interval from the x object. Convert
  (irregular) returns to regular intervals of chosen
  periodicity

  Some description}

  \item{x}{A data.table with columns Instrument, Date,
  Return}

  \item{fill}{How many missing bars to fill/roll backwards
  and forwards?  If not FALSE, a vector of length 2 -
  fill=c(backwards, forwards) E.g. in case of irregularly
  reported weekly data, fill=c(1, 4) will roll Mondays to
  preceding Sundays (weekly endpoints) and Wednesdays to
  the following Sundays}

  \item{to}{Set interval of the resulting time series.}

  \item{eow.fri}{Mark weekly endpoints as Fridays in the
  output.  Internally, weekly endpoints are by default
  Sundays

  Main use is to convert from MTD type returns to periodic
  returns Create Price index

  Add Base Index/Price (for MTD version) Make daily
  timeframe Compress timeframe to lower interval Roll price
  index to the period ends. Set periodicity of the
  resulting time series. TODO: accomodate other input types
  than MTD}
}
\description{
  #Constructor for class 'returns' - time series of returns
  # #Convert object to a class time series of returns # #
}

