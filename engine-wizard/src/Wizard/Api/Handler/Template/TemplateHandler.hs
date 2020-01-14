module Wizard.Api.Handler.Template.TemplateHandler where

import qualified Data.Text as T
import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Service.Template.TemplateService

getTemplatesA :: Endpoint
getTemplatesA =
  checkPermission "DMP_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    mPkgId <- getQueryParam "pkgId"
    eitherResDto <- runInAuthService $ listTemplates (T.unpack <$> mPkgId)
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
