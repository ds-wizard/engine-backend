module Util.Date where

import Data.Maybe
import Data.Time
import qualified Web.JWT as JWT

-- First day in computer calendar
day0 = UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0

-- One Day represented in seconds
nominalDay = 24 * 60 * 60

toNumericDate :: UTCTime -> Maybe JWT.NumericDate
toNumericDate dateTime = JWT.numericDate $ diffUTCTime dateTime day0
