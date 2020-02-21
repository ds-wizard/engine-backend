module Wizard.Api.Handler.Feedback.List_Synchronization_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService

type List_Synchronization_GET
   = Header "Authorization" String
     :> "feedbacks"
     :> "synchronization"
     :> Verb 'GET 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_synchronization_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_synchronization_GET mServiceToken =
  runInUnauthService $
  addTraceUuidHeader =<< do
    checkServiceToken mServiceToken
    synchronizeFeedbacks
    return NoContent
