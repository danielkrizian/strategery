
require(strategery)

newStrategy("tfh")

# select universe
symbols <- "SPX"
Universe(symbols) # prepare ohlc data frame

cal <- time.frame(symbols, bds=TRUE) # trading days calendar

TurnOfMonth <- function(x, last=1, first=3, advance=2) {
  eom <-endpoints(x, on="months") # end of month
  tom <- sort(as.vector(outer(eom, (-last+1 - advance):(first - advance),"+"))) # shift backward&forward
  tom <- tom[tom > 0 & tom <= length(x)] # eliminate values outside range
  1:length(x) %in% tom
}

nmom <- 20
last.days <- 1
first.days <- 3

#Close.Prices <- indicator(Close, input=OHLCV)
TOM <- indicator( TurnOfMonth(Date, last.days, first.days, advance=2), input=cal)
mom <- indicator( momentum(Close, nmom), input=OHLCV)

Long <- (mom>0) %AND% (TOM==TRUE) %position% shares(1) # %buy% equity.pct(2) 
Neutral <- (mom<=0) %OR% (TOM!=TRUE) %position% shares(0)
# Rebalance <- EOM==TRUE %rebalance% equitypct(20) # rebalance rule: each month, rebalance to 20% equity target
Check(plot=F, window="1980")

options("warn"=-1)

bt <- Backtest()

saveStrategy()


newStrategy("fomc")
symbols <- "SPX"
Universe(symbols)

# data 1980-2011 based on Lucca Moench (2012)
# prior to 1994: market infers info from Open Market Operations day following the meeting
# (few occasions the FOMC announced, on the days of meetings)
# exclude c("1968-12-17","1979-09-18", "1979-10-06", "1990-12-18","1996-03-26")
# Between September 1994 and May 1999 statements were released only when a change to the current policy was made. Other- wise, the FOMC announced that no statement would be released, indicating to investors that no policy action had been taken.
# From September 1994 to March 2011, FOMC statements were regularly released at, or a few minutes after, 2:15 pm following each scheduled meeting.7 Since April 2011, the time of the release has varied between 12:30 pm and 2:00 pm on days of FOMC meetings on which a press conference by the FOMC Chairman is held at 2:15 pm.
# Hence, the 2pm-2pm pre-FOMC window that we study in the post-1994
# "Greenbook" rule prior xxxx : The calendar of past FOMC meetings and those scheduled for the next year can be found at www.federalreserve. gov/monetarypolicy/fomccalendars.htm. The website clearly marks conference calls, which are always unscheduled. We distinguish the very infrequent unscheduled meetings not conducted via teleconference from scheduled ones based on whether staff material for FOMC members (the "Greenbook") had been prepared in advance of each meeting.# The evidence prior to 1994 is based on daily data. Over that period we consider as pre-FOMC returns those earned on days of scheduled FOMC meetings, which mark the last trading session
# use only since 1980 (as in study). Prior to 1980 unverified data (Greenbook)
FomcDay <- function(x, before=2) {
  # Only (last day of) scheduled meetings 
  # Prior 1994, Greenbook indicates scheduled meetings 
  # Data prior 1980 unverified and not used in Lucca Moench (2012) study. TODO test prior 1980
  data(fomc)
  fomcDates <- as.Date(as.character(unlist(fomc)))
  fomcDates <- fomcDates[fomcDates>=as.Date("1980-01-01")] # remove unverified data
  fomcDates <- fomcDates[!fomcDates %in% as.Date(c("1968-12-17","1979-09-18",
                                                   "1979-10-06", "1990-12-18","1996-03-26"))] # exclude same-day announcements (see study)
  isFomc <- x %in% fomcDates
  beforeFomc <- c(tail(isFomc, -before), rep(FALSE, before))

  return(beforeFomc)
}

beforeFOMC <- 2

FOMC <- indicator( FomcDay(Date, before=beforeFOMC), input=cal)

Long <- (FOMC==TRUE) %position% shares(1) # %buy% equity.pct(2) 
Neutral <- (FOMC!=TRUE) %position% shares(0)

Backtest()

saveStrategy()