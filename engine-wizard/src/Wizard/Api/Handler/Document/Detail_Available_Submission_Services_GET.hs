module Wizard.Api.Handler.Document.Detail_Available_Submission_Services_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Submission.SubmissionService

type Detail_Available_Submission_Services_GET
   = Header "Authorization" String
     :> "documents"
     :> Capture "docUuuid" String
     :> "available-submission-services"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [SubmissionServiceSimpleDTO])

detail_available_submission_Services_GET ::
     Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [SubmissionServiceSimpleDTO])
detail_available_submission_Services_GET mTokenHeader docUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "SUBM_PERM"
      getAvailableServicesForSubmission docUuid
