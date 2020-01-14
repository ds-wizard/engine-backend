module Registry.Api.Handler.Organization.OrganizationHandler where

import Control.Monad.Reader (liftM)
import qualified Data.Text as T
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationChangeJM ()
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Service.Organization.OrganizationService

getOrganizationsA :: Endpoint
getOrganizationsA =
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDto <- runInAuthService getOrganizations
    case eitherDto of
      Right resDto -> json resDto
      Left error -> sendError error

postOrganizationsA :: Endpoint
postOrganizationsA =
  getReqDto $ \reqDto -> do
    eitherDto <- runInUnauthService $ createOrganization reqDto
    case eitherDto of
      Right resDto -> do
        status created201
        json resDto
      Left error -> sendError error

getOrganizationA :: Endpoint
getOrganizationA =
  getAuthServiceExecutor $ \runInAuthService -> do
    orgId <- param "orgId"
    eitherDto <- runInAuthService $ getOrganizationByOrgId orgId
    case eitherDto of
      Right resDto -> json resDto
      Left error -> sendError error

putOrganizationA :: Endpoint
putOrganizationA =
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      orgId <- param "orgId"
      eitherResDto <- runInAuthService $ modifyOrganization orgId reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error

deleteOrganizationA :: Endpoint
deleteOrganizationA =
  getAuthServiceExecutor $ \runInAuthService -> do
    orgId <- param "orgId"
    maybeError <- runInAuthService $ deleteOrganization orgId
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

changeOrganizationStateA :: Endpoint
changeOrganizationStateA =
  getReqDto $ \reqDto -> do
    orgId <- param "orgId"
    hash <- getQueryParam "hash"
    eitherResDto <- runInUnauthService $ changeOrganizationState orgId (liftM T.unpack hash) reqDto
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error

putOrganizationTokenA :: Endpoint
putOrganizationTokenA = do
  orgId <- param "orgId"
  hash <- getQueryParam "hash"
  eitherResDto <- runInUnauthService $ changeOrganizationTokenByHash orgId (liftM T.unpack hash)
  case eitherResDto of
    Right resDto -> json resDto
    Left error -> sendError error
