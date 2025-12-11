module Wizard.Api.Handler.Project.Detail_Content_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectContentChangeDTO
import Wizard.Api.Resource.Project.ProjectContentChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type Detail_Content_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectContentChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "content"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectContentChangeDTO)

detail_content_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectContentChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectContentChangeDTO)
detail_content_PUT mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyContent uuid reqDto
