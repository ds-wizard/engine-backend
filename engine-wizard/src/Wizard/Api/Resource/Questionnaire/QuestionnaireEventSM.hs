module Wizard.Api.Resource.Questionnaire.QuestionnaireEventSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents

instance ToSchema SetReplyEventDTO where
  declareNamedSchema = simpleToSchema setReplyEvent

instance ToSchema ClearReplyEventDTO where
  declareNamedSchema = simpleToSchema clearReplyEvent

instance ToSchema SetLevelEventDTO where
  declareNamedSchema = simpleToSchema clearReplyEvent

instance ToSchema SetLabelsEventDTO where
  declareNamedSchema = simpleToSchema clearReplyEvent
