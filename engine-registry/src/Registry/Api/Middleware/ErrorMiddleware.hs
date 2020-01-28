module Registry.Api.Middleware.ErrorMiddleware
  ( errorMiddleware
  ) where

import Data.Aeson (encode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Lazy.Char8 as BSL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Monoid ((<>))
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Middleware, Response, responseLBS, responseToStream)
import System.IO.Unsafe (unsafePerformIO)

import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)

errorMiddleware :: Middleware
errorMiddleware application request sendResponse = application request $ sendResponse . process

process :: Response -> Response
process res =
  let (_status, _headers, streamBody) = responseToStream res
   in responseLBS _status (modifyHeaders _headers) (modifyBody _status streamBody)

modifyHeaders :: [Header] -> [Header]
modifyHeaders hs = hs ++ [contentTypeHeaderJSON]

modifyBody _status streamBody =
  case statusCode _status of
    400 ->
      if "Error in" `isPrefixOf` oldBody
        -- It should be retrieved from Localization Dictionary
        then encode $ UserErrorDTO "Problem in deserialization of JSON"
        else oldBody
    _ -> oldBody
  where
    oldBody :: LB.ByteString
    oldBody =
      unsafePerformIO $
      streamBody $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content
