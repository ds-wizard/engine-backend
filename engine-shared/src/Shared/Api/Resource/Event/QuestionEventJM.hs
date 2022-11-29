module Shared.Api.Resource.Event.QuestionEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Event.Question.QuestionEvent
import Shared.Util.Aeson

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddOptionsQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON AddMultiChoiceQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddMultiChoiceQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "MultiChoiceQuestion")]

-- --------------------------------------------
instance FromJSON AddListQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddListQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON AddValueQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddValueQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddIntegrationQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "IntegrationQuestion")]

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditOptionsQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "OptionsQuestion")]

-- --------------------------------------------
instance FromJSON EditMultiChoiceQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditMultiChoiceQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "MultiChoiceQuestion")]

-- --------------------------------------------
instance FromJSON EditListQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditListQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ListQuestion")]

-- --------------------------------------------
instance FromJSON EditValueQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditValueQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ValueQuestion")]

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditIntegrationQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "IntegrationQuestion")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteQuestionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
