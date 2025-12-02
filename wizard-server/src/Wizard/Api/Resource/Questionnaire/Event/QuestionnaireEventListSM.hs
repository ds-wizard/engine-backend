module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventListJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEventList
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireEventList where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventList where
  declareNamedSchema = toSwagger (toSetReplyEventList (sre_rQ1 questionnaire1.uuid) (Just userAlbert))

instance ToSchema ClearReplyEventList where
  declareNamedSchema = toSwagger (toClearReplyEventList (cre_rQ1 questionnaire1.uuid) (Just userAlbert))

instance ToSchema SetPhaseEventList where
  declareNamedSchema = toSwagger (toSetPhaseEventList (sphse_1 questionnaire1.uuid) (Just userAlbert))

instance ToSchema SetLabelsEventList where
  declareNamedSchema = toSwagger (toSetLabelsEventList (slble_rQ2 questionnaire1.uuid) (Just userAlbert))
