module Wizard.Api.Handler.ProjectAction.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Action.ProjectActionChangeDTO
import Wizard.Api.Resource.Project.Action.ProjectActionChangeJM ()
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Api.Resource.Project.Action.ProjectActionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Action.ProjectActionService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectActionChangeDTO
    :> "project-actions"
    :> Capture "id" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectActionDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectActionChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectActionDTO)
detail_PUT mTokenHeader mServerUrl reqDto paId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectAction paId reqDto
