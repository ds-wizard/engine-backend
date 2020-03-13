module Wizard.Api.Handler.Config.List_Application_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigChangeJM ()
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Application_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AppConfigChangeDTO
     :> "configs"
     :> "application"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigDTO)

list_application_PUT ::
     Maybe String -> AppConfigChangeDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigDTO)
list_application_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyAppConfig reqDto
