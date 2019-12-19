module Wizard.Api.Handler.Template.TemplateHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Service.Template.TemplateService

getTemplatesA :: Endpoint
getTemplatesA =
  checkPermission "DMP_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherResDto <- runInAuthService $ listTemplates
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
