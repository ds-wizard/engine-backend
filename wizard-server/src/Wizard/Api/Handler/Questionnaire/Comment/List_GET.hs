module Wizard.Api.Handler.Questionnaire.Comment.List_GET where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> QueryParam "path" String
    :> QueryParam "resolved" Bool
    :> "comments"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (M.Map String [QuestionnaireCommentThreadList]))

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Bool
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (M.Map String [QuestionnaireCommentThreadList]))
list_GET mTokenHeader mServerUrl qtnUuid mPath mResolved =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireCommentsByQuestionnaireUuid qtnUuid mPath mResolved
