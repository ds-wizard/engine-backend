module Wizard.Api.Handler.Config.List_App_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.App.AppConfigService

type List_App_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] AppConfigChangeDTO
    :> "configs"
    :> "app"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] AppConfig)

list_app_PUT
  :: Maybe String
  -> Maybe String
  -> AppConfigChangeDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] AppConfig)
list_app_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyAppConfigDto reqDto
