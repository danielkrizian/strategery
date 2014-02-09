\name{compute.cagr}
\alias{compute.cagr}
\title{Calculate coumpounded annualized growth rate}
\usage{
  compute.cagr(R,
    equity = equity.index(R, continuous = continuous, base = 1, na.fill = list(0, 0, 0)),
    continuous = F, ann = ann.factor("monthly"),
    allow.incomplete = F)
}
\description{
  Missing values treatment: All leading NAs are removed
  (e.g. different inception dates of two aligned return
  series). Trailing NAs are removed (e.g. instruments
  redeemed/sold from the portfolio, but aligned to the
  currently held instruments). Intradata NAs are NOT
  removed - i.e. return
}

