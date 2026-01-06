module Wizard.Api.Handler.ProjectAction.Detail_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Api.Resource.Project.Action.ProjectActionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Action.ProjectActionService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "project-actions"
    :> Capture "id" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectActionDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectActionDTO)
detail_GET mTokenHeader mServerUrl paId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getProjectAction paId
