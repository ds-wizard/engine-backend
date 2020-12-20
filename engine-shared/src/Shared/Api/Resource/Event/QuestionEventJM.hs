module Shared.Api.Resource.Event.QuestionEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Event.Question.QuestionEvent
import Shared.Util.JSON

instance ToJSON AddQuestionEvent where
  toJSON = toSumJSON

instance FromJSON AddQuestionEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "questionType"
    case referenceType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEvent' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (AddMultiChoiceQuestionEvent' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEvent' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEvent' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (AddIntegrationQuestionEvent' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddOptionsQuestionEvent where
  parseJSON = simpleParseJSON "_addOptionsQuestionEvent"

instance ToJSON AddOptionsQuestionEvent where
  toJSON = simpleToJSON'' "_addOptionsQuestionEvent" [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON AddMultiChoiceQuestionEvent where
  parseJSON = simpleParseJSON "_addMultiChoiceQuestionEvent"

instance ToJSON AddMultiChoiceQuestionEvent where
  toJSON = simpleToJSON'' "_addMultiChoiceQuestionEvent" [("questionType", "MultiChoiceQuestion")]

-- --------------------------------------------
instance FromJSON AddListQuestionEvent where
  parseJSON = simpleParseJSON "_addListQuestionEvent"

instance ToJSON AddListQuestionEvent where
  toJSON = simpleToJSON'' "_addListQuestionEvent" [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON AddValueQuestionEvent where
  parseJSON = simpleParseJSON "_addValueQuestionEvent"

instance ToJSON AddValueQuestionEvent where
  toJSON = simpleToJSON'' "_addValueQuestionEvent" [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEvent where
  parseJSON = simpleParseJSON "_addIntegrationQuestionEvent"

instance ToJSON AddIntegrationQuestionEvent where
  toJSON = simpleToJSON'' "_addIntegrationQuestionEvent" [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditQuestionEvent where
  toJSON = toSumJSON

instance FromJSON EditQuestionEvent where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEvent' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (EditMultiChoiceQuestionEvent' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEvent' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEvent' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (EditIntegrationQuestionEvent' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditOptionsQuestionEvent where
  parseJSON = simpleParseJSON "_editOptionsQuestionEvent"

instance ToJSON EditOptionsQuestionEvent where
  toJSON = simpleToJSON'' "_editOptionsQuestionEvent" [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON EditMultiChoiceQuestionEvent where
  parseJSON = simpleParseJSON "_editMultiChoiceQuestionEvent"

instance ToJSON EditMultiChoiceQuestionEvent where
  toJSON = simpleToJSON'' "_editMultiChoiceQuestionEvent" [("questionType", "MultiChoiceQuestion")]

-- --------------------------------------------
instance FromJSON EditListQuestionEvent where
  parseJSON = simpleParseJSON "_editListQuestionEvent"

instance ToJSON EditListQuestionEvent where
  toJSON = simpleToJSON'' "_editListQuestionEvent" [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON EditValueQuestionEvent where
  parseJSON = simpleParseJSON "_editValueQuestionEvent"

instance ToJSON EditValueQuestionEvent where
  toJSON = simpleToJSON'' "_editValueQuestionEvent" [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEvent where
  parseJSON = simpleParseJSON "_editIntegrationQuestionEvent"

instance ToJSON EditIntegrationQuestionEvent where
  toJSON = simpleToJSON'' "_editIntegrationQuestionEvent" [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteQuestionEvent where
  parseJSON = simpleParseJSON "_deleteQuestionEvent"

instance ToJSON DeleteQuestionEvent where
  toJSON = simpleToJSON' "_deleteQuestionEvent" "eventType"
