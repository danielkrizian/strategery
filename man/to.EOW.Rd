\name{to.EOW}
\alias{to.EOW}
\title{Shift date to the nearest end of the week}
\usage{
  to.EOW(x, business = T)
}
\arguments{
  \item{business}{If TRUE, week ends on Friday, else it
  ends on Sunday If FALSE, week ends on Friday}
}
\description{
  Saturdays and Sundays are shifted to the preceding
  Friday. Value is logical, indicating whether the Date
  supplied is at the end of the week
}

