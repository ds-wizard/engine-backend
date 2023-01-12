module Wizard.Util.Date where

import Data.Maybe
import Data.Time
import qualified Web.JWT as JWT

-- First day in computer calendar
day0 = UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0

-- One Hour represented in seconds
nominalHourInSeconds = 60 * 60

toNumericDate :: UTCTime -> Maybe JWT.NumericDate
toNumericDate dateTime = JWT.numericDate $ diffUTCTime dateTime day0
