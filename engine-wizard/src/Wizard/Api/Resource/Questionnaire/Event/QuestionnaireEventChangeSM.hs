module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

instance ToSchema QuestionnaireEventChangeDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_setReplyEventChangeDTO" (toSetReplyEventChangeDTO sre_rQ1)

instance ToSchema ClearReplyEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_clearReplyEventChangeDTO" (toClearReplyEventChangeDTO cre_rQ1)

instance ToSchema SetLevelEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_setLevelEventChangeDTO" (toSetLevelEventChangeDTO slvle_1)

instance ToSchema SetLabelsEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_setLabelsEventChangeDTO" (toSetLabelsEventChangeDTO slble_rQ2)
