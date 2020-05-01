module Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.KnowledgeModelMapper
import Shared.Service.Metric.MetricMapper
import Shared.Util.Swagger

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema KnowledgeModelDTO where
  declareNamedSchema = simpleToSchema (toKnowledgeModelDTO km1)

instance ToSchema KnowledgeModelEntitiesDTO where
  declareNamedSchema = simpleToSchema (toKnowledgeModelEntitiesDTO $ km1 ^. entities)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema ChapterDTO where
  declareNamedSchema = simpleToSchema (toChapterDTO chapter1)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema QuestionValueType

instance ToSchema QuestionDTO

-- --------------------------------------------------------------------
instance ToSchema OptionsQuestionDTO where
  declareNamedSchema = simpleToSchema'' "questionType" "_optionsQuestionDTO" (extract dto)
    where
      extract (OptionsQuestionDTO' e) = e
      dto = toQuestionDTO question2'

-- --------------------------------------------------------------------
instance ToSchema ListQuestionDTO where
  declareNamedSchema = simpleToSchema'' "questionType" "_listQuestionDTO" (extract dto)
    where
      extract (ListQuestionDTO' e) = e
      dto = toQuestionDTO question4'

-- --------------------------------------------------------------------
instance ToSchema ValueQuestionDTO where
  declareNamedSchema = simpleToSchema'' "questionType" "_valueQuestionDTO" (extract dto)
    where
      extract (ValueQuestionDTO' e) = e
      dto = toQuestionDTO question1'

-- --------------------------------------------------------------------
instance ToSchema IntegrationQuestionDTO where
  declareNamedSchema = simpleToSchema'' "questionType" "_integrationQuestionDTO" (extract dto)
    where
      extract (IntegrationQuestionDTO' e) = e
      dto = toQuestionDTO question9'

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema AnswerDTO where
  declareNamedSchema = simpleToSchema (toAnswerDTO q2_answerNo)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema ExpertDTO where
  declareNamedSchema = simpleToSchema (toExpertDTO km1_ch1_q2_eAlbert)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema ReferenceDTO

-- --------------------------------------------------------------------
instance ToSchema ResourcePageReferenceDTO where
  declareNamedSchema = simpleToSchema'' "referenceType" "_resourcePageReferenceDTO" (extract dto)
    where
      extract (ResourcePageReferenceDTO' e) = e
      dto = toReferenceDTO km1_ch1_q2_r1'

-- --------------------------------------------------------------------
instance ToSchema URLReferenceDTO where
  declareNamedSchema = simpleToSchema'' "referenceType" "_uRLReferenceDTO" (extract dto)
    where
      extract (URLReferenceDTO' e) = e
      dto = toReferenceDTO km1_ch1_q2_r2'

-- --------------------------------------------------------------------
instance ToSchema CrossReferenceDTO where
  declareNamedSchema = simpleToSchema'' "referenceType" "_crossReferenceDTO" (extract dto)
    where
      extract (CrossReferenceDTO' e) = e
      dto = toReferenceDTO km1_ch1_q2_r3'

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema MetricDTO where
  declareNamedSchema = simpleToSchema (toMetricDTO metricF)

-- --------------------------------------------------------------------
instance ToSchema MetricMeasureDTO where
  declareNamedSchema = simpleToSchema (toMetricMeasureDTO mm1)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema TagDTO where
  declareNamedSchema = simpleToSchema (toTagDTO tagBioInformatic)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToSchema IntegrationDTO where
  declareNamedSchema = simpleToSchema (toIntegrationDTO bioPortal)
