module Wizard.Api.Handler.Project.Version.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Version.ProjectVersionListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Service.Project.Version.ProjectVersionService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "versions"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [ProjectVersionList])

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [ProjectVersionList])
list_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getVersions uuid
