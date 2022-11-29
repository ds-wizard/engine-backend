module Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM where

import Data.Swagger hiding (Reference, Tag)

import Shared.Api.Resource.Common.MapEntrySM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Swagger

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
  declareNamedSchema = toSwaggerWithType "questionType" (extract dto)
    where
      extract (ListQuestion' e) = e
      dto = question4'

-- --------------------------------------------------------------------
instance ToSchema ValueQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question1

-- --------------------------------------------------------------------
instance ToSchema IntegrationQuestion where
  declareNamedSchema = toSwaggerWithType "questionType" question9

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
instance ToSchema Integration

-- --------------------------------------------------------------------
instance ToSchema ApiIntegration where
  declareNamedSchema = toSwaggerWithType "integrationType" bioPortal

-- --------------------------------------------------------------------
instance ToSchema WidgetIntegration where
  declareNamedSchema = toSwaggerWithType "integrationType" widgetPortal
