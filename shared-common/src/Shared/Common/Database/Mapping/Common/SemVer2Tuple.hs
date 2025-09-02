module Shared.Common.Database.Mapping.Common.SemVer2Tuple where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.String (fromString)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Model.Common.SemVer2Tuple

-- Simple parser for a string like "(1,2)"
parseSemVer :: ByteString -> Either String SemVer2Tuple
parseSemVer = parseOnly $ do
  _ <- char '('
  a <- decimal
  _ <- char ','
  b <- decimal
  _ <- char ')'
  return (SemVer2Tuple a b)

-- FROM database
instance FromField SemVer2Tuple where
  fromField _ (Just bs) = either (error . ("Parse error: " ++)) pure (parseSemVer bs)
  fromField f Nothing = returnError UnexpectedNull f "NULL SemVer2Tuple"

-- TO database
instance ToField SemVer2Tuple where
  toField (SemVer2Tuple a b) =
    let txt = "(" ++ show a ++ "," ++ show b ++ ")"
     in Escape (fromString txt)
