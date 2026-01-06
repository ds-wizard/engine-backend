module Wizard.Api.Handler.Project.List_POST_FromTemplate where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateJM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type List_POST_FromTemplate =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectCreateFromTemplateDTO
    :> "projects"
    :> "from-template"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDTO)

list_POST_FromTemplate
  :: Maybe String
  -> Maybe String
  -> ProjectCreateFromTemplateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectDTO)
list_POST_FromTemplate mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createProjectFromTemplate reqDto
