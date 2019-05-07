module Util.String
  ( splitOn
  ) where

import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs
