module Wizard.Api.Handler.Feedback.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Feedback.FeedbackJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Feedback.FeedbackService

type Detail_GET =
  Header "Host" String
    :> "feedbacks"
    :> Capture "fUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] FeedbackDTO)

detail_GET :: Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] FeedbackDTO)
detail_GET mServerUrl fUuid =
  runInUnauthService mServerUrl NoTransaction $ addTraceUuidHeader =<< getFeedbackByUuid fUuid
