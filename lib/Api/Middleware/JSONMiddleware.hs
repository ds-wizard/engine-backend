module Api.Middleware.JSONMiddleware
  ( jsonMiddleware
  ) where

import Network.HTTP.Types (Header)
import Network.Wai (Middleware)
import Network.Wai.Internal (Response(..))

jsonMiddleware :: Middleware
jsonMiddleware application request sendResponse = application request $ sendResponse . modifyResponse

modifyResponse :: Response -> Response
modifyResponse (ResponseFile s hs b1 b2) = ResponseFile s (modifyHeaders hs) b1 b2
modifyResponse (ResponseBuilder s hs b) = ResponseBuilder s (modifyHeaders hs) b
modifyResponse (ResponseStream s hs b) = ResponseStream s (modifyHeaders hs) b
modifyResponse r@(ResponseRaw _ _) = r

modifyHeaders :: [Header] -> [Header]
modifyHeaders hs = hs ++ jsonHeaders

jsonHeaders = [("Content-Type", "application/json")]
