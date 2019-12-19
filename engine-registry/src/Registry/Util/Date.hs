module Registry.Util.Date where

import Data.Maybe
import Data.Time

-- First day in computer calendar
day0 = UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0

-- One Day represented in seconds
nominalDayInSeconds = 24 * 60 * 60
