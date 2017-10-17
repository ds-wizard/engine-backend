module Api.Handler.Common where

import Data.Aeson
import Network.HTTP.Types.Status (notFound404)
import qualified Web.Scotty as Scotty

notFoundA :: Scotty.ActionM ()
notFoundA = do
  Scotty.status notFound404
  Scotty.json $ object ["status" .= 404, "error" .= "Not Found"]

