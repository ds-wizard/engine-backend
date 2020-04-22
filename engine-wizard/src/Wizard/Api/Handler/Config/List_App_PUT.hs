module Wizard.Api.Handler.Config.List_App_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_App_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AppConfigChangeDTO
     :> "configs"
     :> "app"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfig)

list_app_PUT :: Maybe String -> AppConfigChangeDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfig)
list_app_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyAppConfigDto reqDto
