module Wizard.Api.Handler.QuestionnaireImporter.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaire-importers"
    :> "suggestions"
    :> QueryParam "questionnaireUuid" String
    :> QueryParam "q" String
    :> QueryParam "enabled" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireImporterDTO))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireImporterDTO))
list_suggestions_GET mTokenHeader mServerUrl mQuestionnaireUuid mQuery mEnabled mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getQuestionnaireImporterSuggestions mQuestionnaireUuid mQuery mEnabled (Pageable mPage mSize) (parseSortQuery mSort)
