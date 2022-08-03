module Wizard.Api.Handler.Submission.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Submission.SubmissionService

type List_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "documents"
     :> Capture "docUuid" String
     :> "submissions"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [SubmissionDTO])

list_GET ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [SubmissionDTO])
list_GET mTokenHeader mServerUrl docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getSubmissionsForDocument docUuid
