module Api.Handler.Config.ClientConfigHandler where

import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Web.Scotty.Trans (addHeader, text)

import Api.Handler.Common
import Api.Resource.Config.ClientConfigJM ()
import Service.Config.ClientConfigService

getClientConfigA :: Endpoint
getClientConfigA = do
  mCallbackName <- getQueryParam "callback"
  let callbackName = LT.pack . T.unpack . fromMaybe "callback" $ mCallbackName
  eitherDto <- runInUnauthService getClientConfig
  case eitherDto of
    Right resDto -> do
      let content = decodeUtf8 . encode $ resDto
      addHeader "Content-Type" "application/javascript; charset=utf-8"
      text $ LT.concat [callbackName, "(", content, ")"]
    Left error -> sendError error
