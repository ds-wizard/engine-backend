module Wizard.Api.Handler.Questionnaire.Detail_Settings_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Settings_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireSettingsChangeDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "settings"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireSettingsChangeDTO)

detail_settings_PUT
  :: Maybe String
  -> Maybe String
  -> QuestionnaireSettingsChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireSettingsChangeDTO)
detail_settings_PUT mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyQuestionnaireSettings qtnUuid reqDto
