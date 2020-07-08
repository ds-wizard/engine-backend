module Wizard.Api.Handler.Config.List_App_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_App_GET
   = Header "Authorization" String
     :> "configs"
     :> "app"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfig)

list_app_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfig)
list_app_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService -> runInAuthService $ addTraceUuidHeader =<< getAppConfigDto
