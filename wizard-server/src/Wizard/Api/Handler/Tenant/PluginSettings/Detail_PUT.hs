module Wizard.Api.Handler.Tenant.PluginSettings.Detail_PUT where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.PluginSettings.TenantPluginSettingsService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] A.Value
    :> "tenants"
    :> "current"
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)

detail_PUT :: Maybe String -> Maybe String -> A.Value -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] A.Value)
detail_PUT mTokenHeader mServerUrl reqDto pluginUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< createOrUpdatePluginSettings pluginUuid reqDto
