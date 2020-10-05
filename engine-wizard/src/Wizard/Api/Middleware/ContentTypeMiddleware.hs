module Wizard.Api.Middleware.ContentTypeMiddleware
  ( contentTypeMiddleware
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (Header)
import Network.Wai (Middleware)
import Network.Wai.Internal (Response(..))

contentTypeMiddleware :: Middleware
contentTypeMiddleware application request sendResponse = application request $ sendResponse . modifyResponse

modifyResponse :: Response -> Response
modifyResponse (ResponseFile s hs b1 b2) = ResponseFile s (modifyHeaders hs) b1 b2
modifyResponse (ResponseBuilder s hs b) = ResponseBuilder s (modifyHeaders hs) b
modifyResponse (ResponseStream s hs b) = ResponseStream s (modifyHeaders hs) b
modifyResponse r@(ResponseRaw _ _) = r

modifyHeaders :: [Header] -> [Header]
modifyHeaders hs =
  if length (filter (\(hName, _) -> hName == CI.mk "Content-Type") hs) == 2
    then filter
           (\(hName, hValue) -> not (hName == CI.mk "Content-Type" && hValue == BS.pack "application/octet-stream"))
           hs
    else hs
