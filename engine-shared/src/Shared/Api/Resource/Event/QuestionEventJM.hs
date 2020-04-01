module Shared.Api.Resource.Event.QuestionEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.QuestionEventDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON', simpleToJSON'', toSumJSON)

instance ToJSON AddQuestionEventDTO where
  toJSON = toSumJSON

instance FromJSON AddQuestionEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "questionType"
    case referenceType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEventDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEventDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEventDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (AddIntegrationQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddOptionsQuestionEventDTO where
  parseJSON = simpleParseJSON "_addOptionsQuestionEventDTO"

instance ToJSON AddOptionsQuestionEventDTO where
  toJSON = simpleToJSON'' "_addOptionsQuestionEventDTO" [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON AddListQuestionEventDTO where
  parseJSON = simpleParseJSON "_addListQuestionEventDTO"

instance ToJSON AddListQuestionEventDTO where
  toJSON = simpleToJSON'' "_addListQuestionEventDTO" [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON AddValueQuestionEventDTO where
  parseJSON = simpleParseJSON "_addValueQuestionEventDTO"

instance ToJSON AddValueQuestionEventDTO where
  toJSON = simpleToJSON'' "_addValueQuestionEventDTO" [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEventDTO where
  parseJSON = simpleParseJSON "_addIntegrationQuestionEventDTO"

instance ToJSON AddIntegrationQuestionEventDTO where
  toJSON = simpleToJSON'' "_addIntegrationQuestionEventDTO" [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditQuestionEventDTO where
  toJSON = toSumJSON

instance FromJSON EditQuestionEventDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEventDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEventDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEventDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (EditIntegrationQuestionEventDTO' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditOptionsQuestionEventDTO where
  parseJSON = simpleParseJSON "_editOptionsQuestionEventDTO"

instance ToJSON EditOptionsQuestionEventDTO where
  toJSON = simpleToJSON'' "_editOptionsQuestionEventDTO" [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON EditListQuestionEventDTO where
  parseJSON = simpleParseJSON "_editListQuestionEventDTO"

instance ToJSON EditListQuestionEventDTO where
  toJSON = simpleToJSON'' "_editListQuestionEventDTO" [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON EditValueQuestionEventDTO where
  parseJSON = simpleParseJSON "_editValueQuestionEventDTO"

instance ToJSON EditValueQuestionEventDTO where
  toJSON = simpleToJSON'' "_editValueQuestionEventDTO" [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEventDTO where
  parseJSON = simpleParseJSON "_editIntegrationQuestionEventDTO"

instance ToJSON EditIntegrationQuestionEventDTO where
  toJSON = simpleToJSON'' "_editIntegrationQuestionEventDTO" [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteQuestionEventDTO where
  parseJSON = simpleParseJSON "_deleteQuestionEventDTO"

instance ToJSON DeleteQuestionEventDTO where
  toJSON = simpleToJSON' "_deleteQuestionEventDTO" "eventType"
