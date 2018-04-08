module Api.Handler.ActionKey.ActionKeyHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.ActionKey.ActionKeyDTO
import Common.Context
import Model.Config.DSWConfig
import Common.Error
import Service.User.UserService

postActionKeysA :: Context -> DSWConfig -> Scotty.ActionM ()
postActionKeysA context dswConfig =
  getReqDto $ \reqDto -> do
    maybeError <- liftIO $ resetUserPassword context dswConfig reqDto
    case maybeError of
      Nothing -> Scotty.status created201
      Just error -> sendError error
