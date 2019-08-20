module Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.KnowledgeModel.KnowledgeModel
import Util.JSON (simpleParseJSON, simpleToJSON, simpleToJSON')

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
  toJSON (OptionsQuestionDTO' event) = toJSON event
  toJSON (ListQuestionDTO' event) = toJSON event
  toJSON (ValueQuestionDTO' event) = toJSON event
  toJSON (IntegrationQuestionDTO' event) = toJSON event

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
  toJSON = simpleToJSON' "questionType" "_optionsQuestionDTO"

instance FromJSON OptionsQuestionDTO where
  parseJSON = simpleParseJSON "_optionsQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON ListQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_listQuestionDTO"

instance FromJSON ListQuestionDTO where
  parseJSON = simpleParseJSON "_listQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON ValueQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_valueQuestionDTO"

instance FromJSON ValueQuestionDTO where
  parseJSON = simpleParseJSON "_valueQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_integrationQuestionDTO"

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
  toJSON (ResourcePageReferenceDTO' event) = toJSON event
  toJSON (URLReferenceDTO' event) = toJSON event
  toJSON (CrossReferenceDTO' event) = toJSON event

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
  toJSON = simpleToJSON' "referenceType" "_resourcePageReferenceDTO"

instance FromJSON ResourcePageReferenceDTO where
  parseJSON = simpleParseJSON "_resourcePageReferenceDTO"

-- --------------------------------------------------------------------
instance ToJSON URLReferenceDTO where
  toJSON = simpleToJSON' "referenceType" "_uRLReferenceDTO"

instance FromJSON URLReferenceDTO where
  parseJSON = simpleParseJSON "_uRLReferenceDTO"

-- --------------------------------------------------------------------
instance ToJSON CrossReferenceDTO where
  toJSON = simpleToJSON' "referenceType" "_crossReferenceDTO"

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
