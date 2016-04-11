require(timeDate)
FutureTradingDays <- function(today, days) {
  dates <- today + 1:(days*1.5)
  
  # Remove the Weekends.
  dates <- dates[isWeekday(dates)]
  
  # Get a list of the holidays for the next couple of years and remove them.
  thisYear <- as.numeric(format(Last(today), "%Y"))
  holi <- as.Date(holidayNYSE(thisYear + 0:(days/365+1)))
  dates <- dates[!(dates %in% holi)]
  
  # Now just return the actual days asked for
  return(dates[1:days])
}
