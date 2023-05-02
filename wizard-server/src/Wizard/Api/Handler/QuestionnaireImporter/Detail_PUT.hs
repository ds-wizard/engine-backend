module Wizard.Api.Handler.QuestionnaireImporter.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeJM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireImporterChangeDTO
    :> "questionnaire-importers"
    :> Capture "qiId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireImporterDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> QuestionnaireImporterChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireImporterDTO)
detail_PUT mTokenHeader mServerUrl reqDto qiId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyQuestionnaireImporter qiId reqDto
