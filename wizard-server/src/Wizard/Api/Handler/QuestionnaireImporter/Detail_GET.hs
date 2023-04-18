module Wizard.Api.Handler.QuestionnaireImporter.Detail_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaire-importers"
    :> Capture "qiId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireImporterDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireImporterDTO)
detail_GET mTokenHeader mServerUrl qiId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getQuestionnaireImporter qiId
