module Api.Handler.Organization.OrganizationHandler where

import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Organization.OrganizationDTO ()
import Model.Context.AppContext
import Service.Organization.OrganizationService

getOrganizationCurrentA :: Endpoint
getOrganizationCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  eitherDto <- liftIO $ getOrganization context
  case eitherDto of
    Right resDto -> json resDto
    Left error -> sendError error

putOrganizationCurrentA :: Endpoint
putOrganizationCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "ORG_PERM" $
    getReqDto $ \reqDto -> do
      eitherResDto <- liftIO $ modifyOrganization context reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error
