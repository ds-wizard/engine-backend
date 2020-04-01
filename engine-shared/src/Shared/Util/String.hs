module Shared.Util.String
  ( lowerFirst
  , replace
  , splitOn
  , stripSuffix
  , isSuffixOf
  , format
  ) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [toLower c]
lowerFirst (s:str) = toLower s : str

replace :: String -> String -> String -> String
replace name value string = T.unpack $ T.replace (T.pack name) (T.pack value) (T.pack string)

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix string = T.unpack <$> T.stripSuffix (T.pack suffix) (T.pack string)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix name = T.isSuffixOf (T.pack suffix) (T.pack name)

format :: String -> [String] -> String
format str terms =
  case str of
    '%':'s':rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ format rest (drop 1 terms)
    '%':'%':'s':rest -> '%' : 's' : format rest terms
    a:rest -> a : format rest terms
    [] -> []
