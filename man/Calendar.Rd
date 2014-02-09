\name{calendar}
\alias{calendar}
\title{Construct an Exchange-specific calendar of trading (business) days}
\source{
  NYSE Holidays: http://www.nyse.com/pdfs/closings.pdf

  NYSE Trading Hours (Saturdays before 1952-09-29):
  http://www.nyse.com/pdfs/historical_trading_hours.pdf
}
\usage{
  calendar(exchange = NULL, years = 1800:2015, from, to,
    QuantLib.safe = TRUE)
}
\description{
  Table of exchange definitions - contains trading bars
  Warning: Saturdays before 1952-09-29 not yet accomodated
}

