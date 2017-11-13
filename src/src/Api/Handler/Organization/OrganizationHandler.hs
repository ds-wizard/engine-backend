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
  eitherDto <- liftIO $ getOrganization context
  case eitherDto of
    Right resDto -> sendJson resDto
    Left error -> sendError error

putOrganizationCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
putOrganizationCurrentA context dspConfig =
  checkPermission context "ORG_PERM" $
  getReqDto $ \reqDto -> do
    eitherResDto <- liftIO $ modifyOrganization context reqDto
    case eitherResDto of
      Right resDto -> sendJson resDto
      Left error -> sendError error
