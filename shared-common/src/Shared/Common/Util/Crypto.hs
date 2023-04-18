module Shared.Common.Util.Crypto (
  generateRandomString,
  encryptAES256,
  encryptAES256WithB64,
  decryptAES256,
  decryptAES256WithB64,
  hashMD5,
) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import Crypto.Error (throwCryptoError)
import Crypto.Hash (Digest, MD5 (..), hash)
import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

generateRandomString :: Int -> IO String
generateRandomString lengthOfString =
  BS.unpack . convertToBase Base64URLUnpadded <$> (getRandomBytes lengthOfString :: IO BS.ByteString)

encryptAES256 :: String -> String -> String
encryptAES256 key plainData = BS.unpack $ encryptAES256Raw (BS.pack key) (BS.pack plainData)

encryptAES256WithB64 :: String -> String -> String
encryptAES256WithB64 key plainData =
  if B64.isBase64 (BS.pack plainData)
    then BS.unpack $ encryptAES256Raw (BS.pack key) (B64.decodeBase64Lenient . BS.pack $ plainData)
    else BS.unpack . B64.encodeBase64' $ encryptAES256Raw (BS.pack key) (BS.pack plainData)

encryptAES256Raw :: BS.ByteString -> BS.ByteString -> BS.ByteString
encryptAES256Raw key = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = throwCryptoError $ cipherInit key

decryptAES256 :: String -> String -> String
decryptAES256 = encryptAES256

decryptAES256WithB64 :: String -> String -> String
decryptAES256WithB64 key plainData =
  BS.unpack $ encryptAES256Raw (BS.pack key) (B64.decodeBase64Lenient . BS.pack $ plainData)

hashMD5 :: String -> String
hashMD5 text =
  let digest :: Digest MD5
      digest = hash . TE.encodeUtf8 . T.pack $ text
   in show digest
