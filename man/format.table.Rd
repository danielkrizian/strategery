\name{format.table}
\alias{format.table}
\title{numeric adjustments - round, scientific, negative, as percent
align decimals - always colwise - format()
add marks (big numbers, decimal ...) - always colwise - replace strings
add percent (%) sign to the end - concatenate string
add currency ($, GBP ...) sign to the beginning - concatenate string
TODO add brackets instead of negative sign - concatenate string
output to either graphical device, document or html(webpage)}
\usage{
  format.table(x, by.col = T, special = F, percent = F,
    round = 2, negative = F)
}
\arguments{
  \item{x}{- list, matrix or data frame of statistics. List
  will be converted to matrix}
}
\description{
  "pct" - 0.26% - round(x, digits=2) "ccy" - GBP
  205'265'365
}
\details{
  Format summary data.table or data.frame by column or by
  row Each col/row can be formatted separately

  Arguments support vectors
}

