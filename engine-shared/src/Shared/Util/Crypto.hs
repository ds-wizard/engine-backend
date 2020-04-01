module Shared.Util.Crypto
  ( generateRandomString
  , encryptAES256
  , decryptAES256
  ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV)
import Crypto.Error (throwCryptoError)
import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.ByteString.Char8 as BS

generateRandomString :: Int -> IO String
generateRandomString lengthOfString =
  BS.unpack . convertToBase Base64URLUnpadded <$> (getRandomBytes lengthOfString :: IO BS.ByteString)

encryptAES256 :: String -> String -> String
encryptAES256 key plainData = BS.unpack $ ctrCombine ctx nullIV (BS.pack plainData)
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit (BS.pack key)

decryptAES256 :: String -> String -> String
decryptAES256 = encryptAES256
