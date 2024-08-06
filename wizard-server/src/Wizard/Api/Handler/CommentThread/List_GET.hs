module Wizard.Api.Handler.CommentThread.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadAssignedJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "comment-threads"
    :> QueryParam "q" String
    :> QueryParam "questionnaireUuid" U.UUID
    :> QueryParam "resolved" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireCommentThreadAssigned))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireCommentThreadAssigned))
list_GET mTokenHeader mServerUrl mQuery mQuestionnaireUuid resolved mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getQuestionnaireCommentThreadsPage mQuery mQuestionnaireUuid resolved (Pageable mPage mSize) (parseSortQuery mSort)
