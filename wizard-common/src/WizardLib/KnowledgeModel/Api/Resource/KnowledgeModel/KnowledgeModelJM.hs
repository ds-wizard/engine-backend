module WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModel where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEntities where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelEntities where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Chapter where
  toJSON = genericToJSON jsonOptions

instance FromJSON Chapter where
  parseJSON = genericParseJSON jsonOptions

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
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON OptionsQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON MultiChoiceQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON MultiChoiceQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON ListQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON ListQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON ValueQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON ValueQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON IntegrationQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Answer where
  toJSON = genericToJSON jsonOptions

instance FromJSON Answer where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Choice where
  toJSON = genericToJSON jsonOptions

instance FromJSON Choice where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Expert where
  toJSON = genericToJSON jsonOptions

instance FromJSON Expert where
  parseJSON = genericParseJSON jsonOptions

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
  toJSON = genericToJSON (jsonOptionsWithTypeField "referenceType")

instance FromJSON ResourcePageReference where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "referenceType")

-- --------------------------------------------------------------------
instance ToJSON URLReference where
  toJSON = genericToJSON (jsonOptionsWithTypeField "referenceType")

instance FromJSON URLReference where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "referenceType")

-- --------------------------------------------------------------------
instance ToJSON CrossReference where
  toJSON = genericToJSON (jsonOptionsWithTypeField "referenceType")

instance FromJSON CrossReference where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "referenceType")

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Metric where
  toJSON = genericToJSON jsonOptions

instance FromJSON Metric where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
instance ToJSON MetricMeasure where
  toJSON = genericToJSON jsonOptions

instance FromJSON MetricMeasure where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Phase where
  toJSON = genericToJSON jsonOptions

instance FromJSON Phase where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Tag where
  toJSON = genericToJSON jsonOptions

instance FromJSON Tag where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ResourceCollection where
  toJSON = genericToJSON jsonOptions

instance FromJSON ResourceCollection where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ResourcePage where
  toJSON = genericToJSON jsonOptions

instance FromJSON ResourcePage where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Integration where
  toJSON = toSumJSON

instance FromJSON Integration where
  parseJSON (Object o) = do
    referenceType <- o .: "integrationType"
    case referenceType of
      "ApiIntegration" -> parseJSON (Object o) >>= \event -> return (ApiIntegration' event)
      "WidgetIntegration" -> parseJSON (Object o) >>= \event -> return (WidgetIntegration' event)
      _ -> fail "One of the integrations has unsupported integrationType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ApiIntegration where
  toJSON = genericToJSON (jsonOptionsWithTypeField "integrationType")

instance FromJSON ApiIntegration where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "integrationType")

-- --------------------------------------------------------------------
instance ToJSON WidgetIntegration where
  toJSON = genericToJSON (jsonOptionsWithTypeField "integrationType")

instance FromJSON WidgetIntegration where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "integrationType")
