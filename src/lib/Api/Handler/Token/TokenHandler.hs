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
import Api.Resources.Token.TokenCreateDTO
import Api.Resources.Token.TokenDTO
import Common.Context
import Common.DSPConfig
import Common.Error
import Service.Token.TokenService

postTokenA :: Context -> DSPConfig -> Scotty.ActionM ()
postTokenA context dspConfig =
  getReqDto $ \reqDto -> do
    eitherTokenDto <- liftIO $ getToken context dspConfig reqDto
    case eitherTokenDto of
      Right tokenDto -> do
        Scotty.status created201
        sendJson tokenDto
      Left error -> unauthorizedA
