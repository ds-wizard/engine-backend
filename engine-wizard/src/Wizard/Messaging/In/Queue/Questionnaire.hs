module Wizard.Messaging.In.Queue.Questionnaire
  ( consumeQuestionnaireEventsQueue
  ) where

import Control.Lens ((^.))
import Network.AMQP
import Prelude hiding (log)

import LensesConfig
import Wizard.Messaging.In.Queue.Common
import Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMDTO
import Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMJM ()
import Wizard.Messaging.Route
import Wizard.Model.Context.AppContext
import Wizard.Service.Questionnaire.Compiler.CompilerService

consumeQuestionnaireEventsQueue :: AppContextM (Maybe ConsumerTag)
consumeQuestionnaireEventsQueue = consumeQueue _QUESTIONNAIRE_QUEUE handler

handler :: QuestionnaireEventMDTO -> AppContextM ()
handler reqDto = saveQuestionnaireEvent (reqDto ^. questionnaireUuid) (reqDto ^. event)
