module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireEventChangeDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetReplyEventChangeDTO (sre_rQ1 questionnaire1.uuid))

instance ToSchema ClearReplyEventChangeDTO where
  declareNamedSchema =
    toSwagger (toClearReplyEventChangeDTO (cre_rQ1 questionnaire1.uuid))

instance ToSchema SetPhaseEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetPhaseEventChangeDTO (sphse_1 questionnaire1.uuid))

instance ToSchema SetLabelsEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetLabelsEventChangeDTO (slble_rQ2 questionnaire1.uuid))

instance ToSchema ResolveCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger rtche_rQ1_t1

instance ToSchema ReopenCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger otche_rQ1_t1

instance ToSchema AssignCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger asche_rQ1_t1

instance ToSchema DeleteCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger dtche_rQ1_t1

instance ToSchema AddCommentEventChangeDTO where
  declareNamedSchema = toSwagger acche_rQ2_t1_1

instance ToSchema EditCommentEventChangeDTO where
  declareNamedSchema = toSwagger ecche_rQ1_t1_1

instance ToSchema DeleteCommentEventChangeDTO where
  declareNamedSchema = toSwagger dcche_rQ1_t1_1
