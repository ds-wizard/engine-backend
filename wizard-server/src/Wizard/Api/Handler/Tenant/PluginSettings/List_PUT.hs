module Wizard.Api.Handler.Tenant.PluginSettings.List_PUT where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Plugin.PluginService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] (M.Map U.UUID Bool)
    :> "tenants"
    :> "current"
    :> "plugin-settings"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_PUT :: Maybe String -> Maybe String -> M.Map U.UUID Bool -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        updatePluginsEnabled reqDto
        return NoContent
