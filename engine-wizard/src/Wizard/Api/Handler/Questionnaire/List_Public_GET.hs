module Wizard.Api.Handler.Questionnaire.List_Public_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.PublicQuestionnaire.PublicQuestionnaireService

type List_Public_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> "public"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireDetailDTO)

list_public_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireDetailDTO)
list_public_GET mTokenHeader = runInUnauthService $ addTraceUuidHeader =<< getPublicQuestionnaire
