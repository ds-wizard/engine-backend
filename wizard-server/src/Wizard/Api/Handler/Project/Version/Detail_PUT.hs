module Wizard.Api.Handler.Project.Version.Detail_PUT where

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

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectVersionChangeDTO
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "versions"
    :> Capture "versionUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectVersionList)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectVersionChangeDTO
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectVersionList)
detail_PUT mTokenHeader mServerUrl reqDto projectUuid versionUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyVersion projectUuid versionUuid reqDto
