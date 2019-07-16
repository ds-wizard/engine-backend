module Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO
import Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Util.JSON (simpleParseJSON, simpleToJSON, simpleToJSON')

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON FilledKnowledgeModelDTO where
  toJSON = simpleToJSON "_filledKnowledgeModelDTO"

instance FromJSON FilledKnowledgeModelDTO where
  parseJSON = simpleParseJSON "_filledKnowledgeModelDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON FilledChapterDTO where
  toJSON = simpleToJSON "_filledChapterDTO"

instance FromJSON FilledChapterDTO where
  parseJSON = simpleParseJSON "_filledChapterDTO"

-- --------------------------------------------------------------------
instance ToJSON FilledQuestionDTO where
  toJSON (FilledOptionsQuestionDTO' event) = toJSON event
  toJSON (FilledListQuestionDTO' event) = toJSON event
  toJSON (FilledValueQuestionDTO' event) = toJSON event
  toJSON (FilledIntegrationQuestionDTO' event) = toJSON event

instance FromJSON FilledQuestionDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (FilledOptionsQuestionDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (FilledListQuestionDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (FilledValueQuestionDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (FilledIntegrationQuestionDTO' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON FilledOptionsQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_filledOptionsQuestionDTO"

instance FromJSON FilledOptionsQuestionDTO where
  parseJSON = simpleParseJSON "_filledOptionsQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON FilledListQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_filledListQuestionDTO"

instance FromJSON FilledListQuestionDTO where
  parseJSON = simpleParseJSON "_filledListQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON FilledValueQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_filledValueQuestionDTO"

instance FromJSON FilledValueQuestionDTO where
  parseJSON = simpleParseJSON "_filledValueQuestionDTO"

-- --------------------------------------------------------------------
instance ToJSON FilledIntegrationQuestionDTO where
  toJSON = simpleToJSON' "questionType" "_filledIntegrationQuestionDTO"

instance FromJSON FilledIntegrationQuestionDTO where
  parseJSON = simpleParseJSON "_filledIntegrationQuestionDTO"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON FilledAnswerDTO where
  toJSON = simpleToJSON "_filledAnswerDTO"

instance FromJSON FilledAnswerDTO where
  parseJSON = simpleParseJSON "_filledAnswerDTO"

-- --------------------------------------------------------------------
instance ToJSON FilledAnswerItemDTO where
  toJSON = simpleToJSON "_filledAnswerItemDTO"

instance FromJSON FilledAnswerItemDTO where
  parseJSON = simpleParseJSON "_filledAnswerItemDTO"
