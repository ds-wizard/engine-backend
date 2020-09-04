module Shared.Util.Crypto
  ( generateRandomString
  , encryptAES256
  , decryptAES256
  , hashMD5
  ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV)
import Crypto.Error (throwCryptoError)
import Crypto.Hash (Digest, MD5(..), hash)
import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

hashMD5 :: String -> String
hashMD5 text =
  let digest :: Digest MD5
      digest = hash . TE.encodeUtf8 . T.pack $ text
   in show digest
