module Api.Handler.Token.TokenHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.Token.TokenCreateDTO
import Api.Resource.Token.TokenDTO
import Common.Context
import Common.DSWConfig
import Common.Error
import Service.Token.TokenService

postTokenA :: Context -> DSWConfig -> Scotty.ActionM ()
postTokenA context dswConfig =
  getReqDto $ \reqDto -> do
    eitherTokenDto <- liftIO $ getToken context dswConfig reqDto
    case eitherTokenDto of
      Right tokenDto -> do
        Scotty.status created201
        sendJson tokenDto
      Left error -> unauthorizedA
