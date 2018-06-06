module Api.Handler.Organization.OrganizationHandler where

import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Organization.OrganizationDTO ()
import Service.Organization.OrganizationService

getOrganizationCurrentA :: Endpoint
getOrganizationCurrentA = do
  eitherDto <- lift getOrganization
  case eitherDto of
    Right resDto -> json resDto
    Left error -> sendError error

putOrganizationCurrentA :: Endpoint
putOrganizationCurrentA =
  checkPermission "ORG_PERM" $
  getReqDto $ \reqDto -> do
    eitherResDto <- lift $ modifyOrganization reqDto
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
