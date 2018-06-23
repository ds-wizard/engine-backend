module Utils.Date where

import Data.Time
import Data.Maybe

-- First day in computer calendar
day0 = UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0

-- One Day represented in seconds
nominalDay = 86400
