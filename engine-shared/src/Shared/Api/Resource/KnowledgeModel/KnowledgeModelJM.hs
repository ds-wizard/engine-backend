module Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.JSON (simpleParseJSON, simpleToJSON, simpleToJSON', toSumJSON)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModelDTO where
  toJSON = simpleToJSON "_knowledgeModelDTO"

instance FromJSON KnowledgeModelDTO where
  parseJSON = simpleParseJSON "_knowledgeModelDTO"

instance ToJSON KnowledgeModelEntitiesDTO where
  toJSON = simpleToJSON "_knowledgeModelEntitiesDTO"

instance FromJSON KnowledgeModelEntitiesDTO where
  parseJSON = simpleParseJSON "_knowledgeModelEntitiesDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ChapterDTO where
  toJSON = simpleToJSON "_chapterDTO"

instance FromJSON ChapterDTO where
  parseJSON = simpleParseJSON "_chapterDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

instance ToJSON QuestionDTO where
  toJSON = toSumJSON

instance FromJSON QuestionDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (OptionsQuestionDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (ListQuestionDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (ValueQuestionDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (IntegrationQuestionDTO' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON OptionsQuestionDTO where
  toJSON = simpleToJSON' "_optionsQuestionDTO" "questionType"

instance FromJSON OptionsQuestionDTO where
  parseJSON = simpleParseJSON "_optionsQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON ListQuestionDTO where
  toJSON = simpleToJSON' "_listQuestionDTO" "questionType"

instance FromJSON ListQuestionDTO where
  parseJSON = simpleParseJSON "_listQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON ValueQuestionDTO where
  toJSON = simpleToJSON' "_valueQuestionDTO" "questionType"

instance FromJSON ValueQuestionDTO where
  parseJSON = simpleParseJSON "_valueQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestionDTO where
  toJSON = simpleToJSON' "_integrationQuestionDTO" "questionType"

instance FromJSON IntegrationQuestionDTO where
  parseJSON = simpleParseJSON "_integrationQuestionDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON AnswerDTO where
  toJSON = simpleToJSON "_answerDTO"

instance FromJSON AnswerDTO where
  parseJSON = simpleParseJSON "_answerDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ExpertDTO where
  toJSON = simpleToJSON "_expertDTO"

instance FromJSON ExpertDTO where
  parseJSON = simpleParseJSON "_expertDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ReferenceDTO where
  toJSON = toSumJSON

instance FromJSON ReferenceDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (ResourcePageReferenceDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (URLReferenceDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (CrossReferenceDTO' event)
      _ -> fail "One of the references has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ResourcePageReferenceDTO where
  toJSON = simpleToJSON' "_resourcePageReferenceDTO" "referenceType"

instance FromJSON ResourcePageReferenceDTO where
  parseJSON = simpleParseJSON "_resourcePageReferenceDTO"

-- --------------------------------------------------------------------
instance ToJSON URLReferenceDTO where
  toJSON = simpleToJSON' "_uRLReferenceDTO" "referenceType"

instance FromJSON URLReferenceDTO where
  parseJSON = simpleParseJSON "_uRLReferenceDTO"

-- --------------------------------------------------------------------
instance ToJSON CrossReferenceDTO where
  toJSON = simpleToJSON' "_crossReferenceDTO" "referenceType"

instance FromJSON CrossReferenceDTO where
  parseJSON = simpleParseJSON "_crossReferenceDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON MetricDTO where
  toJSON = simpleToJSON "_metricDTO"

instance FromJSON MetricDTO where
  parseJSON = simpleParseJSON "_metricDTO"

-- --------------------------------------------------------------------
instance ToJSON MetricMeasureDTO where
  toJSON = simpleToJSON "_metricMeasureDTO"

instance FromJSON MetricMeasureDTO where
  parseJSON = simpleParseJSON "_metricMeasureDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON TagDTO where
  toJSON = simpleToJSON "_tagDTO"

instance FromJSON TagDTO where
  parseJSON = simpleParseJSON "_tagDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON IntegrationDTO where
  toJSON = simpleToJSON "_integrationDTO"

instance FromJSON IntegrationDTO where
  parseJSON = simpleParseJSON "_integrationDTO"
