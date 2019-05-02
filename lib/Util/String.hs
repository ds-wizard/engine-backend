module Util.String
  ( lowerFirst
  , splitOn
  ) where

import Data.Char (toLower)
import qualified Data.Text as T

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [toLower c]
lowerFirst (s:str) = toLower s : str

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs
