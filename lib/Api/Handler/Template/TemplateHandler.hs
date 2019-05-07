module Api.Handler.Template.TemplateHandler where

import Web.Scotty.Trans (json)

import Api.Handler.Common
import Service.Template.TemplateService

getTemplatesA :: Endpoint
getTemplatesA =
  checkPermission "DMP_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherResDto <- runInAuthService $ listTemplates
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
