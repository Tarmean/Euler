import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

sundays = length $ do
  year <- [1901..2000]
  month <- [1..12]
  let (_, _, 7) = toWeekDate $ fromGregorian year month 1
  return ()
