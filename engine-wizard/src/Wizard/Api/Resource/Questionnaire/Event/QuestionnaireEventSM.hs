module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

instance ToSchema QuestionnaireEventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventDTO where
  declareNamedSchema = simpleToSchema' "_setReplyEventDTO" (toSetReplyEventDTO sre_rQ1 (Just userAlbert))

instance ToSchema ClearReplyEventDTO where
  declareNamedSchema = simpleToSchema' "_clearReplyEventDTO" (toClearReplyEventDTO cre_rQ1 (Just userAlbert))

instance ToSchema SetLevelEventDTO where
  declareNamedSchema = simpleToSchema' "_setLevelEventDTO" (toSetLevelEventDTO slvle_1 (Just userAlbert))

instance ToSchema SetLabelsEventDTO where
  declareNamedSchema = simpleToSchema' "_setLabelsEventDTO" (toSetLabelsEventDTO slble_rQ2 (Just userAlbert))
