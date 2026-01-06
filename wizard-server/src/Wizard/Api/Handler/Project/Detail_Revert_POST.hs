module Wizard.Api.Handler.Project.Detail_Revert_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Api.Resource.Project.ProjectContentJM ()
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Version.ProjectVersionService

type Detail_Revert_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectVersionRevertDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "revert"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectContentDTO)

detail_revert_POST
  :: Maybe String
  -> Maybe String
  -> ProjectVersionRevertDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectContentDTO)
detail_revert_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< revertToEvent uuid reqDto True
