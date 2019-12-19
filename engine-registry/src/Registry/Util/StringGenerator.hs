module Registry.Util.StringGenerator
  ( generateRandomString
  ) where

import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.ByteString.Char8 as BS

generateRandomString :: Int -> IO String
generateRandomString lengthOfString =
  (getRandomBytes lengthOfString :: IO BS.ByteString) >>= return . BS.unpack . convertToBase Base64URLUnpadded
