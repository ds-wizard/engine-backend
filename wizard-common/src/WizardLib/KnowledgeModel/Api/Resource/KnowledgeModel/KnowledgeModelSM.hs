module WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM where

import Data.Swagger hiding (Reference, Tag)

import Shared.Common.Api.Resource.Common.MapEntrySM ()
import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Choices
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Metrics
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Resources
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema KnowledgeModel where
  declareNamedSchema = toSwagger km1

instance ToSchema KnowledgeModelEntities where
  declareNamedSchema = toSwagger km1.entities

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Chapter where
  declareNamedSchema = toSwagger chapter1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema QuestionValueType

instance ToSchema Question

-- --------------------------------------------------------------------
instance ToSchema OptionsQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question2

-- --------------------------------------------------------------------
instance ToSchema MultiChoiceQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question11

-- --------------------------------------------------------------------
instance ToSchema ListQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question4'

-- --------------------------------------------------------------------
instance ToSchema ValueQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question1

instance ToSchema QuestionValidation where
  declareNamedSchema = toSwaggerWithFlatType "type" OrcidQuestionValidation

-- --------------------------------------------------------------------
instance ToSchema IntegrationQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question9

-- --------------------------------------------------------------------
instance ToSchema ItemSelectQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question13

-- --------------------------------------------------------------------
instance ToSchema FileQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question14

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Answer where
  declareNamedSchema = toSwagger q2_answerNo

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Choice where
  declareNamedSchema = toSwagger q11_choice1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Expert where
  declareNamedSchema = toSwagger km1_ch1_q2_eAlbert

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Reference

-- --------------------------------------------------------------------
instance ToSchema ResourcePageReference where
  declareNamedSchema = toSwaggerWithType "referenceType" km1_ch1_q2_r1

-- --------------------------------------------------------------------
instance ToSchema URLReference where
  declareNamedSchema = toSwaggerWithType "referenceType" km1_ch1_q2_r2

-- --------------------------------------------------------------------
instance ToSchema CrossReference where
  declareNamedSchema = toSwaggerWithType "referenceType" km1_ch1_q2_r3

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Metric where
  declareNamedSchema = toSwagger metricF

-- --------------------------------------------------------------------
instance ToSchema MetricMeasure where
  declareNamedSchema = toSwagger mm1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Phase where
  declareNamedSchema = toSwagger phase1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Tag where
  declareNamedSchema = toSwagger tagBioInformatic

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema ResourceCollection where
  declareNamedSchema = toSwagger rc1

instance ToSchema ResourcePage where
  declareNamedSchema = toSwagger rc1_rp1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Integration

-- --------------------------------------------------------------------
instance ToSchema ApiIntegration where
  declareNamedSchema = toSwaggerWithType "integrationType" repositoryApi

instance ToSchema TypeHintExchange where
  declareNamedSchema = toSwagger repositoryApiTypeHintExchange1

instance ToSchema TypeHintRequest where
  declareNamedSchema = toSwagger repositoryApiTypeHintRequest1

instance ToSchema TypeHintResponse where
  declareNamedSchema = toSwagger typeHintResponse1

instance ToSchema SuccessTypeHintResponse where
  declareNamedSchema = toSwagger successTypeHintResponse1

instance ToSchema RemoteErrorTypeHintResponse where
  declareNamedSchema = toSwagger remoteErrorTypeHintResponse1

instance ToSchema RequestFailedTypeHintResponse where
  declareNamedSchema = toSwagger requestFailedTypeHintResponse1

-- --------------------------------------------------------------------
instance ToSchema ApiLegacyIntegration where
  declareNamedSchema = toSwaggerWithType "integrationType" bioPortal

-- --------------------------------------------------------------------
instance ToSchema WidgetIntegration where
  declareNamedSchema = toSwaggerWithType "integrationType" widgetPortal
