module Api.Handler.Common where

import Data.ByteString
import Data.Aeson
import Network.HTTP.Types (hContentType, notFound404)
import Network.HTTP.Types.Status (unauthorized401, notFound404)
import Network.Wai (Response, responseLBS)
import qualified Web.Scotty as Scotty

applicationJson :: ByteString
applicationJson = "application/json; charset=utf-8"

unauthorizedA :: Scotty.ActionM ()
unauthorizedA = do
  Scotty.status unauthorized401
  Scotty.json $ object ["status" .= 401, "error" .= "Unauthorized"]

unauthorizedL :: Response
unauthorizedL = responseLBS unauthorized401 [(hContentType, applicationJson)] $ encode (object ["status" .= 401, "error" .= "Unauthorized"])

notFoundA :: Scotty.ActionM ()
notFoundA = do
  Scotty.status notFound404
  Scotty.json $ object ["status" .= 404, "error" .= "Not Found"]
