module Wizard.Api.Handler.QuestionnaireAction.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.QuestionnaireAction.QuestionnaireActionService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaire-actions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireActionDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireActionDTO))
list_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getQuestionnaireActionsPageDto mQuery (Pageable mPage mSize) (parseSortQuery mSort)
