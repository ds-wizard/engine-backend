module Wizard.Api.Handler.Template.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_GET
   = Header "Authorization" String
     :> "templates"
     :> QueryParam "pkgId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [TemplateDTO])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [TemplateDTO])
list_GET mTokenHeader mPkgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "DMP_PERM"
      getTemplatesDto mPkgId
