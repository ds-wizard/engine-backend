module Api.Handler.ActionKey.ActionKeyHandler where

import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (status)

import Api.Handler.Common
import Api.Resource.ActionKey.ActionKeyJM ()
import Service.User.UserService

postActionKeysA :: Endpoint
postActionKeysA =
  getReqDto $ \reqDto -> do
    maybeError <- runInUnauthService $ resetUserPassword reqDto
    case maybeError of
      Nothing -> status created201
      Just error -> sendError error
