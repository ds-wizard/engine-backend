module Wizard.Api.Handler.Document.Detail_Available_Submission_Services_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Submission.SubmissionService

type Detail_Available_Submission_Services_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> Capture "docUuid" String
    :> "available-submission-services"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [SubmissionServiceSimpleDTO])

detail_available_submission_Services_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [SubmissionServiceSimpleDTO])
detail_available_submission_Services_GET mTokenHeader mServerUrl docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAvailableServicesForSubmission docUuid
