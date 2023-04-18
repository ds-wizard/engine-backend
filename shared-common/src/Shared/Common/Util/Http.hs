module Shared.Common.Util.Http where

import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (mk)
import Data.List (find)
import Network.HTTP.Types (Header, Status)
import Network.Wai (Request (..), Response)
import Network.Wai.Internal (Response (..))

findHeader :: String -> [Header] -> Maybe String
findHeader headerNameToFind headers =
  case find (\(headerName, headerValue) -> headerName == (mk . BS.pack $ headerNameToFind)) headers of
    Just (_, headerValue) -> Just . BS.unpack $ headerValue
    Nothing -> Nothing

extractPath :: Request -> String
extractPath request = (BS.unpack . rawPathInfo $ request) ++ (BS.unpack . rawQueryString $ request)

extractMethod :: Request -> String
extractMethod = BS.unpack . requestMethod

processHeaderInMiddleware :: (Request -> Status -> [Header] -> [Header]) -> Request -> Response -> Response
processHeaderInMiddleware processFunction request (ResponseFile status headers b1 b2) =
  let modifiedHeaders = processFunction request status headers
   in ResponseFile status modifiedHeaders b1 b2
processHeaderInMiddleware processFunction request (ResponseBuilder status headers b) =
  let modifiedHeaders = processFunction request status headers
   in ResponseBuilder status modifiedHeaders b
processHeaderInMiddleware processFunction request (ResponseStream status headers b) =
  let modifiedHeaders = processFunction request status headers
   in ResponseStream status modifiedHeaders b
processHeaderInMiddleware processFunction request r@(ResponseRaw _ _) = r
