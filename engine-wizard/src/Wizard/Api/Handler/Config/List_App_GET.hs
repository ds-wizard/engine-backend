module Wizard.Api.Handler.Config.List_App_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.App.AppConfigService

type List_App_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "configs"
    :> "app"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] AppConfig)

list_app_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] AppConfig)
list_app_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAppConfigDto
