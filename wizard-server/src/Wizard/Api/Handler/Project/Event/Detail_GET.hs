module Wizard.Api.Handler.Project.Event.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Event.ProjectEventDTO
import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "events"
    :> Capture "eventUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectEventDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectEventDTO)
detail_GET mTokenHeader mServerUrl projectUuid eventUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectEventForProjectUuid projectUuid eventUuid
