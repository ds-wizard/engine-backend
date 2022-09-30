module Shared.Util.Date where

import Data.Maybe (fromJust)
import Data.Time

dt' :: Integer -> Int -> Int -> UTCTime
dt' year month day = UTCTime (fromJust $ fromGregorianValid year month day) 0

dt'' :: Integer -> Int -> Int -> Integer -> UTCTime
dt'' year month day secsFromMidnight =
  UTCTime (fromJust $ fromGregorianValid year month day) (secondsToDiffTime secsFromMidnight)

parsePostgresDateTime :: String -> UTCTime
parsePostgresDateTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q+00"
