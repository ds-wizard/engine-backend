module Wizard.Api.Handler.ActionKey.ActionKeyHandler where

import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (status)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.ActionKey.ActionKeyJM ()
import Wizard.Service.User.UserService

postActionKeysA :: Endpoint
postActionKeysA =
  getReqDto $ \reqDto -> do
    maybeError <- runInUnauthService $ resetUserPassword reqDto
    case maybeError of
      Nothing -> status created201
      Just error -> sendError error
