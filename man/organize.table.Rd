\name{organize.table}
\alias{organize.table}
\title{Subset, sort, relabel, transpose a table}
\usage{
  organize.table(x, select.rows, select.cols,
    subset = NULL, sort = NULL, ascending = NULL,
    row.names = rownames(x), col.names = colnames(x),
    metrics.in.rows = TRUE)
}
\arguments{
  \item{x}{- list, matrix or data frame of statistics. List
  will be converted to matrix}

  \item{select.rows,}{select.cols - character vector
  specifying which rows/cols to only display as row names
  and col names}

  \item{subset}{- TODO. currently only select rows is
  enabled}

  \item{metrics.in.rows}{- supports both tables with
  metrics in rows or in columns}
}
\description{
  Subset, sort, relabel, transpose a table
}

