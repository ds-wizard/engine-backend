module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

instance ToSchema QuestionnaireEventChangeDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetReplyEventChangeDTO sre_rQ1 samplePhasesAnsweredIndication)

instance ToSchema ClearReplyEventChangeDTO where
  declareNamedSchema =
    toSwagger (toClearReplyEventChangeDTO cre_rQ1 samplePhasesAnsweredIndication)

instance ToSchema SetPhaseEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetPhaseEventChangeDTO sphse_1 samplePhasesAnsweredIndication)

instance ToSchema SetLabelsEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetLabelsEventChangeDTO slble_rQ2 samplePhasesAnsweredIndication)

instance ToSchema ResolveCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger rtche_rQ1_t1

instance ToSchema ReopenCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger otche_rQ1_t1

instance ToSchema DeleteCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger dtche_rQ1_t1

instance ToSchema AddCommentEventChangeDTO where
  declareNamedSchema = toSwagger acche_rQ2_t1_1

instance ToSchema EditCommentEventChangeDTO where
  declareNamedSchema = toSwagger ecche_rQ1_t1_1

instance ToSchema DeleteCommentEventChangeDTO where
  declareNamedSchema = toSwagger dcche_rQ1_t1_1
