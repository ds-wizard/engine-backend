module Api.Handler.Common where

import Data.Aeson
import Data.ByteString
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (hContentType, notFound404)
import Network.HTTP.Types.Method (methodOptions)
import Network.HTTP.Types.Status
       (ok200, badRequest400, unauthorized401, notFound404)
import Network.Wai
import qualified Web.Scotty as Scotty

sendJson obj = do
  Scotty.setHeader (LT.pack "Content-Type") (LT.pack "application/json")
  Scotty.raw $ encode obj

unauthorizedA :: Scotty.ActionM ()
unauthorizedA = do
  Scotty.status unauthorized401
  sendJson $ object ["status" .= 401, "error" .= "Unauthorized"]

badRequest :: String -> Scotty.ActionM ()
badRequest message = do
  Scotty.status badRequest400
  sendJson $
    object ["status" .= 400, "error" .= "Bad Request", "message" .= message]

unauthorizedL :: Response
unauthorizedL =
  responseLBS unauthorized401 [(hContentType, "application/json")] $
  encode (object ["status" .= 401, "error" .= "Unauthorized"])

notFoundA :: Scotty.ActionM ()
notFoundA = do
  request <- Scotty.request
  if requestMethod request == methodOptions
    then Scotty.status ok200
    else do
      Scotty.status notFound404
      sendJson $ object ["status" .= 404, "error" .= "Not Found"]
