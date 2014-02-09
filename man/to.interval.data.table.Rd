\name{to.interval.data.table}
\alias{to.interval.data.table}
\title{TODO: check SPX data monthly, appears to be buggy}
\usage{
  to.interval.data.table(x,
    to = c("days", "weeks", "months", "quarters", "years"),
    full = FALSE,
    fill = if (to == "weeks") c(1, 4) else if (to == "months") c(3, 3) else FALSE,
    business.week = T)
}
\arguments{
  \item{full}{Align to full calendar frame? If FALSE, just
  filter the observations}
}
\description{
  TODO: check SPX data monthly, appears to be buggy
}

