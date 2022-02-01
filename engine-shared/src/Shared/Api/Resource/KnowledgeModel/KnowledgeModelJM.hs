module Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.JSON

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModel where
  toJSON = simpleToJSON "_knowledgeModel"

instance FromJSON KnowledgeModel where
  parseJSON = simpleParseJSON "_knowledgeModel"

instance ToJSON KnowledgeModelEntities where
  toJSON = simpleToJSON "_knowledgeModelEntities"

instance FromJSON KnowledgeModelEntities where
  parseJSON = simpleParseJSON "_knowledgeModelEntities"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Chapter where
  toJSON = simpleToJSON "_chapter"

instance FromJSON Chapter where
  parseJSON = simpleParseJSON "_chapter"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

instance ToJSON Question where
  toJSON = toSumJSON

instance FromJSON Question where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (OptionsQuestion' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (MultiChoiceQuestion' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (ListQuestion' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (ValueQuestion' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (IntegrationQuestion' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON OptionsQuestion where
  toJSON = simpleToJSON' "_optionsQuestion" "questionType"

instance FromJSON OptionsQuestion where
  parseJSON = simpleParseJSON "_optionsQuestion"

-- --------------------------------------------------------------------
instance ToJSON MultiChoiceQuestion where
  toJSON = simpleToJSON' "_multiChoiceQuestion" "questionType"

instance FromJSON MultiChoiceQuestion where
  parseJSON = simpleParseJSON "_multiChoiceQuestion"

-- --------------------------------------------------------------------
instance ToJSON ListQuestion where
  toJSON = simpleToJSON' "_listQuestion" "questionType"

instance FromJSON ListQuestion where
  parseJSON = simpleParseJSON "_listQuestion"

-- --------------------------------------------------------------------
instance ToJSON ValueQuestion where
  toJSON = simpleToJSON' "_valueQuestion" "questionType"

instance FromJSON ValueQuestion where
  parseJSON = simpleParseJSON "_valueQuestion"

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestion where
  toJSON = simpleToJSON' "_integrationQuestion" "questionType"

instance FromJSON IntegrationQuestion where
  parseJSON = simpleParseJSON "_integrationQuestion"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Answer where
  toJSON = simpleToJSON "_answer"

instance FromJSON Answer where
  parseJSON = simpleParseJSON "_answer"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Choice where
  toJSON = simpleToJSON "_choice"

instance FromJSON Choice where
  parseJSON = simpleParseJSON "_choice"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Expert where
  toJSON = simpleToJSON "_expert"

instance FromJSON Expert where
  parseJSON = simpleParseJSON "_expert"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Reference where
  toJSON = toSumJSON

instance FromJSON Reference where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (ResourcePageReference' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (URLReference' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (CrossReference' event)
      _ -> fail "One of the references has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ResourcePageReference where
  toJSON = simpleToJSON' "_resourcePageReference" "referenceType"

instance FromJSON ResourcePageReference where
  parseJSON = simpleParseJSON "_resourcePageReference"

-- --------------------------------------------------------------------
instance ToJSON URLReference where
  toJSON = simpleToJSON' "_uRLReference" "referenceType"

instance FromJSON URLReference where
  parseJSON = simpleParseJSON "_uRLReference"

-- --------------------------------------------------------------------
instance ToJSON CrossReference where
  toJSON = simpleToJSON' "_crossReference" "referenceType"

instance FromJSON CrossReference where
  parseJSON = simpleParseJSON "_crossReference"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Metric where
  toJSON = simpleToJSON "_metric"

instance FromJSON Metric where
  parseJSON = simpleParseJSON "_metric"

-- --------------------------------------------------------------------
instance ToJSON MetricMeasure where
  toJSON = simpleToJSON "_metricMeasure"

instance FromJSON MetricMeasure where
  parseJSON = simpleParseJSON "_metricMeasure"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Phase where
  toJSON = simpleToJSON "_phase"

instance FromJSON Phase where
  parseJSON = simpleParseJSON "_phase"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Tag where
  toJSON = simpleToJSON "_tag"

instance FromJSON Tag where
  parseJSON = simpleParseJSON "_tag"

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Integration where
  toJSON = simpleToJSON "_integration"

instance FromJSON Integration where
  parseJSON = simpleParseJSON "_integration"
