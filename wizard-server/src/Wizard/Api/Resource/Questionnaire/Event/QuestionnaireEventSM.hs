module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireEventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventDTO where
  declareNamedSchema = toSwagger (toSetReplyEventDTO (sre_rQ1 questionnaire1.uuid) (Just userAlbert))

instance ToSchema ClearReplyEventDTO where
  declareNamedSchema = toSwagger (toClearReplyEventDTO (cre_rQ1 questionnaire1.uuid) (Just userAlbert))

instance ToSchema SetPhaseEventDTO where
  declareNamedSchema = toSwagger (toSetPhaseEventDTO (sphse_1 questionnaire1.uuid) (Just userAlbert))

instance ToSchema SetLabelsEventDTO where
  declareNamedSchema = toSwagger (toSetLabelsEventDTO (slble_rQ2 questionnaire1.uuid) (Just userAlbert))

instance ToSchema ResolveCommentThreadEventDTO where
  declareNamedSchema = toSwagger rte_rQ1_t1

instance ToSchema ReopenCommentThreadEventDTO where
  declareNamedSchema = toSwagger ote_rQ1_t1

instance ToSchema AssignCommentThreadEventDTO where
  declareNamedSchema = toSwagger aste_rQ1_t1

instance ToSchema DeleteCommentThreadEventDTO where
  declareNamedSchema = toSwagger dte_rQ1_t1

instance ToSchema AddCommentEventDTO where
  declareNamedSchema = toSwagger ace_rQ1_t1_1

instance ToSchema EditCommentEventDTO where
  declareNamedSchema = toSwagger ece_rQ1_t1_1

instance ToSchema DeleteCommentEventDTO where
  declareNamedSchema = toSwagger dce_rQ1_t1_1
