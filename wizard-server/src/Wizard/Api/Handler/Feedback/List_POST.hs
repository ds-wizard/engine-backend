module Wizard.Api.Handler.Feedback.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackCreateJM ()
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Feedback.FeedbackJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] FeedbackCreateDTO
    :> "feedbacks"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] FeedbackDTO)

list_POST :: Maybe String -> Maybe String -> FeedbackCreateDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] FeedbackDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createFeedback reqDto
