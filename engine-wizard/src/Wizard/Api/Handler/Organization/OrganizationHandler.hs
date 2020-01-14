module Wizard.Api.Handler.Organization.OrganizationHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Organization.OrganizationChangeJM ()
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Service.Organization.OrganizationService

getOrganizationCurrentA :: Endpoint
getOrganizationCurrentA =
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDto <- runInAuthService getOrganization
    case eitherDto of
      Right resDto -> json resDto
      Left error -> sendError error

putOrganizationCurrentA :: Endpoint
putOrganizationCurrentA =
  checkPermission "ORG_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherResDto <- runInAuthService $ modifyOrganization reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error
