module Wizard.Api.Handler.Project.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectCreateJM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectCreateDTO
    :> "projects"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> ProjectCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< createProject reqDto
