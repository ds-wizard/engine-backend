module Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM where

import Control.Lens ((^.))
import Data.Swagger hiding (Reference, Tag)

import LensesConfig
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Swagger

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema KnowledgeModel where
  declareNamedSchema = simpleToSchema' "_knowledgeModel" km1

instance ToSchema KnowledgeModelEntities where
  declareNamedSchema = simpleToSchema' "_knowledgeModelEntities" (km1 ^. entities)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Chapter where
  declareNamedSchema = simpleToSchema' "_chapter" chapter1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema QuestionValueType

instance ToSchema Question

-- --------------------------------------------------------------------
instance ToSchema OptionsQuestion where
  declareNamedSchema = simpleToSchema'' "_optionsQuestion" "questionType" question2

-- --------------------------------------------------------------------
instance ToSchema MultiChoiceQuestion where
  declareNamedSchema = simpleToSchema'' "_multiChoiceQuestion" "questionType" question11

-- --------------------------------------------------------------------
instance ToSchema ListQuestion where
  declareNamedSchema = simpleToSchema'' "_listQuestion" "questionType" (extract dto)
    where
      extract (ListQuestion' e) = e
      dto = question4'

-- --------------------------------------------------------------------
instance ToSchema ValueQuestion where
  declareNamedSchema = simpleToSchema'' "_valueQuestion" "questionType" question1

-- --------------------------------------------------------------------
instance ToSchema IntegrationQuestion where
  declareNamedSchema = simpleToSchema'' "_integrationQuestion" "questionType" question9

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Answer where
  declareNamedSchema = simpleToSchema' "_answer" q2_answerNo

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Choice where
  declareNamedSchema = simpleToSchema' "_choice" q11_choice1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Expert where
  declareNamedSchema = simpleToSchema' "_expert" km1_ch1_q2_eAlbert

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Reference

-- --------------------------------------------------------------------
instance ToSchema ResourcePageReference where
  declareNamedSchema = simpleToSchema'' "_resourcePageReference" "referenceType" km1_ch1_q2_r1

-- --------------------------------------------------------------------
instance ToSchema URLReference where
  declareNamedSchema = simpleToSchema'' "_uRLReference" "referenceType" km1_ch1_q2_r2

-- --------------------------------------------------------------------
instance ToSchema CrossReference where
  declareNamedSchema = simpleToSchema'' "_crossReference" "referenceType" km1_ch1_q2_r3

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Metric where
  declareNamedSchema = simpleToSchema' "_metrics" metricF

-- --------------------------------------------------------------------
instance ToSchema MetricMeasure where
  declareNamedSchema = simpleToSchema' "_metricMeasure" mm1

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Tag where
  declareNamedSchema = simpleToSchema' "_tag" tagBioInformatic

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema Integration where
  declareNamedSchema = simpleToSchema' "_integration" bioPortal
