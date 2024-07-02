module Wizard.Api.Handler.Questionnaire.Detail_Settings_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Settings_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "settings"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailSettings)

detail_settings_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailSettings)
detail_settings_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireDetailSettingsById qtnUuid
