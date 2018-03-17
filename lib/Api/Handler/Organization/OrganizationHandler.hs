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
import Api.Resource.Organization.OrganizationDTO
import Common.Context
import Common.DSWConfig
import Service.Organization.OrganizationService

getOrganizationCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
getOrganizationCurrentA context dswConfig = do
  eitherDto <- liftIO $ getOrganization context
  case eitherDto of
    Right resDto -> sendJson resDto
    Left error -> sendError error

putOrganizationCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
putOrganizationCurrentA context dswConfig =
  checkPermission context "ORG_PERM" $
  getReqDto $ \reqDto -> do
    eitherResDto <- liftIO $ modifyOrganization context reqDto
    case eitherResDto of
      Right resDto -> sendJson resDto
      Left error -> sendError error
