module Wizard.Api.Handler.Document.Detail_Available_Submission_Services_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Wizard.Service.Submission.SubmissionService

type Detail_Available_Submission_Services_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> Capture "docUuid" U.UUID
    :> "available-submission-services"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TenantConfigSubmissionServiceSimple])

detail_available_submission_Services_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TenantConfigSubmissionServiceSimple])
detail_available_submission_Services_GET mTokenHeader mServerUrl docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAvailableServicesForSubmission docUuid
