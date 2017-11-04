module Api.Middleware.CORS (corsMiddleware) where

import Network.HTTP.Types (Header)
import Network.Wai (Middleware)
import Network.Wai.Internal (Response(..))

corsMiddleware :: Middleware
corsMiddleware application request sendResponse =
  application request $ sendResponse . removeHeader

removeHeader :: Response -> Response
removeHeader (ResponseFile s h b1 b2) = ResponseFile s (addCorsHeaders h) b1 b2
removeHeader (ResponseBuilder s h b) = ResponseBuilder s (addCorsHeaders h) b
removeHeader (ResponseStream s h b) = ResponseStream s (addCorsHeaders h) b
removeHeader r@(ResponseRaw _ _) = r

addCorsHeaders :: [Header] -> [Header]
addCorsHeaders hs = hs ++ corsHeaders

corsHeaders =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Credential", "true")
  , ("Access-Control-Allow-Headers", "*")
  , ("Access-Control-Allow-Methods", "OPTIONS, HEAD, GET, POST, PUT, DELETE")
  ]
