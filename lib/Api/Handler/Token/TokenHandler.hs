module Api.Handler.Token.TokenHandler where

import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (json, status)

import Api.Handler.Common
import Model.Context.AppContext
import Service.Token.TokenService

postTokenA :: Endpoint
postTokenA =
  getReqDto $ \reqDto -> do
    dswConfig <- lift . asks $ _appContextConfig
    context <- lift . asks $ _appContextOldContext
    eitherTokenDto <- liftIO $ getToken context dswConfig reqDto
    case eitherTokenDto of
      Right tokenDto -> do
        status created201
        json tokenDto
      Left error -> sendError error
