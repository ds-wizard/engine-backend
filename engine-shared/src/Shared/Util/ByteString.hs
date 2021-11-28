module Shared.Util.ByteString where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

toByteString :: BSB.Builder -> BS.ByteString
toByteString x = BSL.toStrict (BSB.toLazyByteString x)
