module Wizard.Api.Handler.Config.List_Application_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Application_GET
   = Header "Authorization" String
     :> "configs"
     :> "application"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigDTO)

list_application_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigDTO)
list_application_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      getAppConfig
