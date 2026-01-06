module Wizard.Api.Handler.Project.Detail_Settings_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Detail.ProjectDetailSettingsJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Detail.ProjectDetailSettings
import Wizard.Service.Project.ProjectService

type Detail_Settings_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "settings"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailSettings)

detail_settings_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectDetailSettings)
detail_settings_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectDetailSettingsById uuid
