#
# Get a vector of trading days. These are week days, with holidays removed. The trading days
# are relative to the New York Stock Exchange (NYSE).
#
# startDate and endDate are Date objects. The function returns a date ordered vector of Date
# objects which are the trading days.
#
# This function requires the timeDate package
#
# Credit to Ian Kaplan, I modified syntax styling and renamed the function (tradingCalendar)
# Source: http://www.bearcave.com/finance/random_r_hacks/
trading_calendar <- function( startDate, endDate)
{
  require(timeDate)
  timeSeq <- timeSequence(from = as.character(startDate),
                         to = as.character(endDate), 
                         by="day", format = "%Y-%m-%d",
                         zone = "NewYork", FinCenter = "America/New_York")
  
  ix          <- as.logical(isWeekday(timeSeq, wday = 1:5))
  tradingDays <- timeSeq[ix]
  startYear   <- as.POSIXlt(startDate)$year + 1900
  endYear     <- as.POSIXlt(endDate)$year + 1900
  
  tradingDays.dt <- as.Date(tradingDays)
  hol <- as.Date(holidayNYSE(startYear:endYear))
  ix  <- which(tradingDays.dt %in% hol)
  tradingDays.dt <- tradingDays.dt[-ix]
  return(tradingDays.dt)
} # tradingCalendar
