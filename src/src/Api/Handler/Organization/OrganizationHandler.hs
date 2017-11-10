module Api.Handler.Organization.OrganizationHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.Organization.OrganizationDTO
import Context
import DSPConfig
import Service.Organization.OrganizationService

getOrganizationCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
getOrganizationCurrentA context dspConfig = do
  maybeDto <- liftIO $ getOrganization context
  case maybeDto of
    Just dto -> sendJson dto
    _ -> notFoundA

putOrganizationCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
putOrganizationCurrentA context dspConfig = do
  organizationCreateDto <- Scotty.jsonData
  organizationDto <- liftIO $ modifyOrganization context organizationCreateDto
  sendJson organizationDto
