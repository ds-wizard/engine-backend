module Wizard.Api.Handler.QuestionnaireImporter.List_GET where

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

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaire-importers"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireImporterDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireImporterDTO))
list_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getQuestionnaireImportersPageDto mQuery (Pageable mPage mSize) (parseSortQuery mSort)
