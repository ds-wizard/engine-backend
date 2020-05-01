module Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.JSON

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModelDTO where
  toJSON = simpleToJSON "_knowledgeModelDTO"

instance FromJSON KnowledgeModelDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON KnowledgeModelEntitiesDTO where
  toJSON = simpleToJSON "_knowledgeModelEntitiesDTO"

instance FromJSON KnowledgeModelEntitiesDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ChapterDTO where
  toJSON = simpleToJSON "_chapterDTO"

instance FromJSON ChapterDTO where
  parseJSON = genericParseJSON simpleOptions

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
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON ListQuestionDTO where
  toJSON = simpleToJSON' "_listQuestionDTO" "questionType"

instance FromJSON ListQuestionDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON ValueQuestionDTO where
  toJSON = simpleToJSON' "_valueQuestionDTO" "questionType"

instance FromJSON ValueQuestionDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestionDTO where
  toJSON = simpleToJSON' "_integrationQuestionDTO" "questionType"

instance FromJSON IntegrationQuestionDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON AnswerDTO where
  toJSON = simpleToJSON "_answerDTO"

instance FromJSON AnswerDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ExpertDTO where
  toJSON = simpleToJSON "_expertDTO"

instance FromJSON ExpertDTO where
  parseJSON = genericParseJSON simpleOptions

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
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON URLReferenceDTO where
  toJSON = simpleToJSON' "_uRLReferenceDTO" "referenceType"

instance FromJSON URLReferenceDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON CrossReferenceDTO where
  toJSON = simpleToJSON' "_crossReferenceDTO" "referenceType"

instance FromJSON CrossReferenceDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON MetricDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON MetricDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON MetricMeasureDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON MetricMeasureDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON TagDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON TagDTO where
  parseJSON = genericParseJSON simpleOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON IntegrationDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON IntegrationDTO where
  parseJSON = genericParseJSON simpleOptions
