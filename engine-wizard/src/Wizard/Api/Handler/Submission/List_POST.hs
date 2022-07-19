module Wizard.Api.Handler.Submission.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Submission.SubmissionService

type List_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] SubmissionCreateDTO
     :> "documents"
     :> Capture "docUuid" String
     :> "submissions"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] SubmissionDTO)

list_POST ::
     Maybe String
  -> Maybe String
  -> SubmissionCreateDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] SubmissionDTO)
list_POST mTokenHeader mServerUrl reqDto docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< submitDocument docUuid reqDto
