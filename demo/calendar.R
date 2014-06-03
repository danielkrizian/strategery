require(strategery);   require(rChartsDygraphs)
options(lazy.indicators=T)
options(param.indicators=T)

##### Turn Of The Month ######

# This strategy goes long the S&P 500 index around the turn of each month 
# (last trading day and first three trading days). 
# Momentum filter applied, i.e. price must be greater than 20 days ago.
newStrategy("tom")

Universe("SPX") # prepare ohlcv data

# trading days calendar of the universe instruments 
CAL <- Calendar(trading=T) # slow part is timeDate::holidayNYSE

TurnOfMonth <- function(x, last=1, first=3, advance=2) {
  eom <-endpoints(x, on="months") # end of month
  tom <- sort(as.vector(outer(eom, (-last+1 - advance):(first - advance),"+"))) # shift backward&forward
  tom <- tom[tom > 0 & tom <= length(x)] # eliminate values outside range
  1:length(x) %in% tom
}

nmom <- 20
last.days <- 1
first.days <- 3

Close = indicator( Close, data=OHLCV)
TOM = indicator( TurnOfMonth(Date, last.days, first.days, advance=2), data=CAL)
mom = indicator( momentum(Close, nmom), data=OHLCV)

Long <- (mom>0) %AND% (TOM==TRUE) %position% shares(1)
Neutral <- (mom<=0) %OR% (TOM!=TRUE) %position% shares(0)

Visualize()

Backtest()

saveStrategy()

##### FOMC ######

newStrategy("fomc")
Universe("SPX")

# data 1980-2011 based on Lucca Moench (2012)
# prior to 1994: market infers info from Open Market Operations day following the meeting
# (few occasions the FOMC announced, on the days of meetings)
# exclude c("1968-12-17","1979-09-18", "1979-10-06", "1990-12-18","1996-03-26")
# Between September 1994 and May 1999 statements were released only when a change to the current policy was made. 
#Other- wise, the FOMC announced that no statement would be released, indicating to investors that no policy 
#action had been taken.
# From September 1994 to March 2011, FOMC statements were regularly released at, or a few minutes after, 2:15 pm 
#following each scheduled meeting.7 Since April 2011, the time of the release has varied between 12:30 pm and 2:00 
#pm on days of FOMC meetings on which a press conference by the FOMC Chairman is held at 2:15 pm.
# Hence, the 2pm-2pm pre-FOMC window that we study in the post-1994
# "Greenbook" rule prior xxxx : The calendar of past FOMC meetings and those scheduled for the next year can be 
#found at www.federalreserve. gov/monetarypolicy/fomccalendars.htm. The website clearly marks conference calls, 
#which are always unscheduled. We distinguish the very infrequent unscheduled meetings not conducted via 
#teleconference from scheduled ones based on whether staff material for FOMC members (the "Greenbook") had been 
#prepared in advance of each meeting.# The evidence prior to 1994 is based on daily data. Over that period we 
#consider as pre-FOMC returns those earned on days of scheduled FOMC meetings, which mark the last trading session
# use only since 1980 (as in study). Prior to 1980 unverified data (Greenbook)
FomcDay <- function(x, before=2 ) {
  # Only (last day of) scheduled meetings 
  # Prior 1994, Greenbook indicates scheduled meetings 
  # Data prior 1980 unverified and not used in Lucca Moench (2012) study. TODO test prior 1980
  data(fomc)
  fomcDates <- as.Date(as.character(unlist(fomc)))
  fomcDates <- fomcDates[fomcDates>=as.Date("1980-01-01")] # remove unverified data
  # exclude same-day announcements (see study)
  fomcDates <- fomcDates[!fomcDates %in% as.Date(c("1968-12-17","1979-09-18",
                                                   "1979-10-06", "1990-12-18","1996-03-26"))] 
  isFomc <- x %in% fomcDates
  beforeFomc <- c(tail(isFomc, -before), rep(FALSE, before))
  
  return(beforeFomc)
}

beforeFOMC <- 2

FOMC <- indicator( FomcDay(Date, before=beforeFOMC), data=CAL)

Long <- (FOMC==TRUE) %position% shares(1)
Neutral <- (FOMC!=TRUE) %position% shares(0)

Backtest()

saveStrategy()

##### Holiday effect #####
# Tsiakas(2010) tested since 1962 that: preholidays, postholidays, and prelong weekends 
# have significantly higher mean and lower volatility than regular trading days
# whereas postlong weekends have lower mean and higher volatility.

newStrategy("holiday")
Universe("SPX")
require(timeDate)

FULLCAL <- Calendar(trading=T)

Holiday <- function(x, before=2) {
  as.IDate(holidayNYSE(1800:2015))
}

# on the OHLCV frame, mark preholidays as TRUE, others as FALSE
HOL <- Calendar(holidays=T)[,PreHoliday:=TRUE] # slow part is timeDate::holidayNYSE
PREHOL <- HOL[CAL[,list(Instrument,Date)],roll=-1][is.na(PreHoliday),PreHoliday:=FALSE]

advanceBarsForSig <- 2 # anticipate signal n days in advance
preholiday <- indicator( anticipate(PreHoliday, advanceBarsForSig, pad=F), data=PREHOL)

Long <- (preholiday==TRUE) %position% shares(1)
Neutral <- (preholiday!=TRUE) %position% shares(0)

Backtest()
