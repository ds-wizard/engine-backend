module Wizard.Api.Handler.Submission.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Submission.SubmissionService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] SubmissionCreateDTO
     :> "submissions"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] SubmissionDTO)

list_POST ::
     Maybe String -> SubmissionCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] SubmissionDTO)
list_POST mTokenHeader reqDto =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $ addTraceUuidHeader =<< submitDocument reqDto
