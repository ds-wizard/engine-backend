module Wizard.Api.Handler.Feedback.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackCreateJM ()
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Feedback.FeedbackJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService

type List_POST
   = ReqBody '[ SafeJSON] FeedbackCreateDTO
     :> "feedbacks"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] FeedbackDTO)

list_POST :: FeedbackCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] FeedbackDTO)
list_POST reqDto = runInUnauthService $ addTraceUuidHeader =<< createFeedback reqDto
