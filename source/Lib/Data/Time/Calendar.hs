module Lib.Data.Time.Calendar
  ( monthsBetween,
  )
where

import Data.Time.Calendar

monthsBetween :: Day -> Day -> Integer
monthsBetween start end =
  let (startYear, startMonth, _) = toGregorian start
      (endYear, endMonth, _) = toGregorian end
      yearDiff = endYear - startYear
      monthDiff = fromIntegral endMonth - fromIntegral startMonth
   in yearDiff * 12 + monthDiff
