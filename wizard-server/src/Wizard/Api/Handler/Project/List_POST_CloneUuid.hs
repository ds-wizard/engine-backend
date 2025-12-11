module Wizard.Api.Handler.Project.List_POST_CloneUuid where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type List_POST_CloneUuid =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "clone"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDTO)

list_POST_CloneUuid
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectDTO)
list_POST_CloneUuid mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< cloneProject uuid
