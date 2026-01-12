module Wizard.Api.Handler.User.PluginSettings.Detail_GET where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.PluginSettings.UserPluginSettingsService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)

detail_GET :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] A.Value)
detail_GET mTokenHeader mServerUrl pluginUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getPluginSettings pluginUuid
