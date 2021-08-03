module Wizard.Api.Handler.Questionnaire.Detail_Squash_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService

type Detail_Squash_POST
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "squash"
     :> Verb 'POST 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_squash_POST :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_squash_POST mServiceToken qtnUuid =
  runInUnauthService $
  addTraceUuidHeader =<< do
    checkServiceToken mServiceToken
    squashQuestionnaireEventsForQuestionnaire qtnUuid
    return NoContent
