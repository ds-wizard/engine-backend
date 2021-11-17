module Wizard.Api.Handler.Feedback.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Feedback.FeedbackJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService

type List_GET
   = Header "Host" String
     :> "feedbacks"
     :> QueryParam "packageId" String
     :> QueryParam "questionUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [FeedbackDTO])

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [FeedbackDTO])
list_GET mServerUrl mPackageId mQuestionUuid =
  runInUnauthService mServerUrl $
  addTraceUuidHeader =<< do
    let queryParams = catMaybes [(,) "package_id" <$> mPackageId, (,) "question_uuid" <$> mQuestionUuid]
    getFeedbacksFiltered queryParams
