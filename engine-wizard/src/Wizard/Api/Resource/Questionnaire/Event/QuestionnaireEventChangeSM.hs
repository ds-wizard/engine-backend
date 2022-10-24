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
    simpleToSchema' "_setReplyEventChangeDTO" (toSetReplyEventChangeDTO sre_rQ1 samplePhasesAnsweredIndication)

instance ToSchema ClearReplyEventChangeDTO where
  declareNamedSchema =
    simpleToSchema' "_clearReplyEventChangeDTO" (toClearReplyEventChangeDTO cre_rQ1 samplePhasesAnsweredIndication)

instance ToSchema SetPhaseEventChangeDTO where
  declareNamedSchema =
    simpleToSchema' "_setPhaseEventChangeDTO" (toSetPhaseEventChangeDTO sphse_1 samplePhasesAnsweredIndication)

instance ToSchema SetLabelsEventChangeDTO where
  declareNamedSchema =
    simpleToSchema' "_setLabelsEventChangeDTO" (toSetLabelsEventChangeDTO slble_rQ2 samplePhasesAnsweredIndication)

instance ToSchema ResolveCommentThreadEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_resolveCommentThreadEventChangeDTO" rtche_rQ1_t1

instance ToSchema ReopenCommentThreadEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_reopenCommentThreadEventChangeDTO" otche_rQ1_t1

instance ToSchema DeleteCommentThreadEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_deleteCommentEventThreadChangeDTO" dtche_rQ1_t1

instance ToSchema AddCommentEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_addCommentEventChangeDTO" acche_rQ2_t1_1

instance ToSchema EditCommentEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_editCommentEventChangeDTO" ecche_rQ1_t1_1

instance ToSchema DeleteCommentEventChangeDTO where
  declareNamedSchema = simpleToSchema' "_deleteCommentEventChangeDTO" dcche_rQ1_t1_1
