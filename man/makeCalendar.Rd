\name{makeCalendar}
\alias{makeCalendar}
\title{Generate All calendar dates}
\usage{
  makeCalendar(y = 1800:1915, from, to,
    QuantLib.safe = TRUE,
    retclass = c("data.table", "Date", "xts"),
    auto.assign = F, env = .GlobalEnv)
}
\description{
  If retclass=xts, returns xts object named "Day", with
  zeroes. Construct a calendar of all days
}

