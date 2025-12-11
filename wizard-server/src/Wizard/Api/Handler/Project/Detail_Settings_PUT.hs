module Wizard.Api.Handler.Project.Detail_Settings_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Api.Resource.Project.ProjectSettingsChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type Detail_Settings_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectSettingsChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "settings"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectSettingsChangeDTO)

detail_settings_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectSettingsChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectSettingsChangeDTO)
detail_settings_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectSettings uuid reqDto
