module Wizard.Api.Handler.Submission.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Submission.SubmissionList
import Wizard.Service.Submission.SubmissionService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] SubmissionCreateDTO
    :> "documents"
    :> Capture "docUuid" U.UUID
    :> "submissions"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] SubmissionList)

list_POST
  :: Maybe String
  -> Maybe String
  -> SubmissionCreateDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] SubmissionList)
list_POST mTokenHeader mServerUrl reqDto docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< submitDocument docUuid reqDto
