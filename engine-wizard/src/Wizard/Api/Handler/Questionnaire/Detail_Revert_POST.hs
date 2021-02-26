module Wizard.Api.Handler.Questionnaire.Detail_Revert_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type Detail_Revert_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] QuestionnaireVersionRevertDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "revert"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireContentDTO)

detail_revert_POST ::
     Maybe String
  -> QuestionnaireVersionRevertDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireContentDTO)
detail_revert_POST mTokenHeader reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< revertToEvent qtnUuid reqDto True
