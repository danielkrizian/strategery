\name{returns}
\alias{returns}
\title{Constructor for returns object}
\usage{
  returns(..., symbol = NULL, rf = NULL, bench = NULL,
    as.interval = "irregular", date.col = "Date",
    asset.col = "Instrument", val.col = "Return")
}
\arguments{
  \item{symbol}{name for the instrument, if not supplied
  within ... object.}

  \item{...}{Multi-instrument or single instrument data.}

  \item{rf}{character list of Rf and benchmarks are stored
  as list(InstrumentName=Rf) or list(InstrumentName=Rb)
  pairs in the attributes and their return data aligned by
  Date and rbinded for later retrieval}
}
\description{
  Header is Instrument, Date, Return. returns are in
  data.table with either single or multi-assets, either
  tall or wide format
}

