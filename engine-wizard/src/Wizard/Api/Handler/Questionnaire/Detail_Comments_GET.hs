module Wizard.Api.Handler.Questionnaire.Detail_Comments_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Comments_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "comments"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireCommentListDTO)

detail_comments_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireCommentListDTO)
detail_comments_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireCommentsForQtnUuid qtnUuid
