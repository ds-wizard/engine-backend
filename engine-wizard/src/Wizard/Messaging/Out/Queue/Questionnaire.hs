module Wizard.Messaging.Out.Queue.Questionnaire where

import Data.Aeson (encode)

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Messaging.Out.Queue.Common
import Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMDTO
import Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMJM ()
import Wizard.Messaging.Route
import Wizard.Model.Context.AppContext

publishToQuestionnaireEventsQueue :: String -> QuestionnaireEventDTO -> AppContextM ()
publishToQuestionnaireEventsQueue qtnUuid event = do
  let dto =
        QuestionnaireEventMDTO
          {_questionnaireEventMDTOQuestionnaireUuid = qtnUuid, _questionnaireEventMDTOEvent = event}
  publishToQueue _QUESTIONNAIRE_QUEUE (encode dto)
  return ()
