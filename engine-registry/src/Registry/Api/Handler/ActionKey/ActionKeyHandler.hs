module Registry.Api.Handler.ActionKey.ActionKeyHandler where

import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (status)

import Registry.Api.Handler.Common
import Registry.Api.Resource.ActionKey.ActionKeyJM ()
import Registry.Service.Organization.OrganizationService

postActionKeysA :: Endpoint
postActionKeysA =
  getReqDto $ \reqDto -> do
    maybeError <- runInUnauthService $ resetOrganizationToken reqDto
    case maybeError of
      Nothing -> status created201
      Just error -> sendError error
