module Wizard.Api.Handler.Project.Version.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeJM ()
import Wizard.Api.Resource.Project.Version.ProjectVersionListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Service.Project.Version.ProjectVersionService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectVersionChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "versions"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectVersionList)

list_POST
  :: Maybe String
  -> Maybe String
  -> ProjectVersionChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectVersionList)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createVersion uuid reqDto
