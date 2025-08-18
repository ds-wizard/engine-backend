module Wizard.Api.Handler.User.List_Current_Submission_Props_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserSubmissionPropJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.UserSubmissionPropList
import Wizard.Service.User.Profile.UserProfileService

type List_Current_Submission_Props_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] [UserSubmissionPropList]
    :> "users"
    :> "current"
    :> "submission-props"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserSubmissionPropList])

list_current_submission_props_PUT
  :: Maybe String
  -> Maybe String
  -> [UserSubmissionPropList]
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [UserSubmissionPropList])
list_current_submission_props_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyUserProfileSubmissionProps reqDto
