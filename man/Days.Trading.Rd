\name{Days.Trading}
\alias{Days.Trading}
\title{Construct an Exchange-specific calendar of trading (business) days}
\source{
  NYSE Holidays: http://www.nyse.com/pdfs/closings.pdf

  NYSE Trading Hours (Saturdays before 1952-09-29):
  http://www.nyse.com/pdfs/historical_trading_hours.pdf
}
\usage{
  Days.Trading(exchange = "NYSE")
}
\description{
  Warning: Saturdays before 1952-09-29 not yet accomodated
}

