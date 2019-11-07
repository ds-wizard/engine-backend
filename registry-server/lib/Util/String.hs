module Util.String
  ( generateRandomString
  , lowerFirst
  , replace
  , splitOn
  , stripSuffix
  ) where

import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.Text as T

generateRandomString :: Int -> IO String
generateRandomString lengthOfString =
  (getRandomBytes lengthOfString :: IO BS.ByteString) >>= return . BS.unpack . convertToBase Base64URLUnpadded

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
stripSuffix suffix string = T.unpack <$> (T.stripSuffix (T.pack suffix) (T.pack string))
