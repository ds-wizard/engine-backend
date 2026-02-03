module Wizard.Api.Handler.Project.Event.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Event.ProjectEventService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectEventChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "events"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST
  :: Maybe String
  -> Maybe String
  -> ProjectEventChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        addEventToProject uuid reqDto
        return NoContent
