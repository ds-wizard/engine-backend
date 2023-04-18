module Wizard.Api.Handler.QuestionnaireImporter.List_Suggestions_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
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
    :> QueryParam "questionnaireUuid" U.UUID
    :> QueryParam "q" String
    :> QueryParam "enabled" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireImporterDTO))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe U.UUID
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
