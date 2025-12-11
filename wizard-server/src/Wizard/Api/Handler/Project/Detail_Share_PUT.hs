module Wizard.Api.Handler.Project.Detail_Share_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectShareChangeDTO
import Wizard.Api.Resource.Project.ProjectShareChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type Detail_Share_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectShareChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "share"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectShareChangeDTO)

detail_share_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectShareChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectShareChangeDTO)
detail_share_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectShare uuid reqDto
