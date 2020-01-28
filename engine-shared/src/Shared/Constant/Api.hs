module Shared.Constant.Api where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.Header (HeaderName)

authorizationHeaderName :: String
authorizationHeaderName = "Authorization"

xTraceUuidHeaderName :: String
xTraceUuidHeaderName = "x-trace-uuid"

xUserCountHeaderName :: String
xUserCountHeaderName = "x-user-count"

xPkgCountHeaderName :: String
xPkgCountHeaderName = "x-pkg-count"

xQtnCountHeaderName :: String
xQtnCountHeaderName = "x-qtn-count"

contentTypeHeaderJSON :: (HeaderName, BS.ByteString)
contentTypeHeaderJSON = ("Content-Type", "application/json;charset=utf-8")
