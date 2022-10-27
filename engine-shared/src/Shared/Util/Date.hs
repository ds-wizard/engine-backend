module Shared.Util.Date where

import qualified Data.Attoparsec.Text as AT
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time
import Data.Time.Parsers

dt' :: Integer -> Int -> Int -> UTCTime
dt' year month day = UTCTime (fromJust $ fromGregorianValid year month day) 0

dt'' :: Integer -> Int -> Int -> Integer -> UTCTime
dt'' year month day secsFromMidnight =
  UTCTime (fromJust $ fromGregorianValid year month day) (secondsToDiffTime secsFromMidnight)

parsePostgresDateTime :: String -> Either String UTCTime
parsePostgresDateTime = AT.parseOnly utcTime . T.pack

parsePostgresDateTime' :: String -> UTCTime
parsePostgresDateTime' dateTimeS =
  let (Right dateTime) = parsePostgresDateTime dateTimeS
   in dateTime
