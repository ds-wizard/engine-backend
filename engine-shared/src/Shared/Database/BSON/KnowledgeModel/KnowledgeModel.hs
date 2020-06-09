module Shared.Database.BSON.KnowledgeModel.KnowledgeModel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel

instance FromBSON KnowledgeModel

instance ToBSON KnowledgeModelEntities

instance FromBSON KnowledgeModelEntities

-- -------------------------
-- CHAPTER -----------------
-- -------------------------
instance ToBSON Chapter

instance FromBSON Chapter

-- -------------------------
-- QUESTION ----------------
-- -------------------------
instance ToBSON Question where
  toBSON (OptionsQuestion' event) = toBSON event
  toBSON (ListQuestion' event) = toBSON event
  toBSON (ValueQuestion' event) = toBSON event
  toBSON (IntegrationQuestion' event) = toBSON event

instance FromBSON Question where
  fromBSON doc = do
    questionType <- BSON.lookup "questionType" doc
    case questionType of
      "OptionsQuestion" -> OptionsQuestion' <$> (fromBSON doc :: Maybe OptionsQuestion)
      "ListQuestion" -> ListQuestion' <$> (fromBSON doc :: Maybe ListQuestion)
      "ValueQuestion" -> ValueQuestion' <$> (fromBSON doc :: Maybe ValueQuestion)
      "IntegrationQuestion" -> IntegrationQuestion' <$> (fromBSON doc :: Maybe IntegrationQuestion)

-- ------------------------------------------------
instance ToBSON OptionsQuestion where
  toBSON OptionsQuestion {..} =
    [ "questionType" BSON.=: "OptionsQuestion"
    , "uuid" BSON.=: _optionsQuestionUuid
    , "title" BSON.=: _optionsQuestionTitle
    , "text" BSON.=: _optionsQuestionText
    , "requiredLevel" BSON.=: _optionsQuestionRequiredLevel
    , "tagUuids" BSON.=: _optionsQuestionTagUuids
    , "expertUuids" BSON.=: _optionsQuestionExpertUuids
    , "referenceUuids" BSON.=: _optionsQuestionReferenceUuids
    , "answerUuids" BSON.=: _optionsQuestionAnswerUuids
    ]

instance FromBSON OptionsQuestion

-- ------------------------------------------------
instance ToBSON ListQuestion where
  toBSON ListQuestion {..} =
    [ "questionType" BSON.=: "ListQuestion"
    , "uuid" BSON.=: _listQuestionUuid
    , "title" BSON.=: _listQuestionTitle
    , "text" BSON.=: _listQuestionText
    , "requiredLevel" BSON.=: _listQuestionRequiredLevel
    , "tagUuids" BSON.=: _listQuestionTagUuids
    , "expertUuids" BSON.=: _listQuestionExpertUuids
    , "referenceUuids" BSON.=: _listQuestionReferenceUuids
    , "itemTemplateQuestionUuids" BSON.=: _listQuestionItemTemplateQuestionUuids
    ]

instance FromBSON ListQuestion

-- ------------------------------------------------
instance BSON.Val QuestionValueType where
  val StringQuestionValueType = BSON.String "StringQuestionValueType"
  val NumberQuestionValueType = BSON.String "NumberQuestionValueType"
  val DateQuestionValueType = BSON.String "DateQuestionValueType"
  val TextQuestionValueType = BSON.String "TextQuestionValueType"
  cast' (BSON.String "StringQuestionValueType") = Just StringQuestionValueType
  cast' (BSON.String "NumberQuestionValueType") = Just NumberQuestionValueType
  cast' (BSON.String "DateQuestionValueType") = Just DateQuestionValueType
  cast' (BSON.String "TextQuestionValueType") = Just TextQuestionValueType
  cast' _ = Nothing

instance ToBSON ValueQuestion where
  toBSON ValueQuestion {..} =
    [ "questionType" BSON.=: "ValueQuestion"
    , "uuid" BSON.=: _valueQuestionUuid
    , "title" BSON.=: _valueQuestionTitle
    , "text" BSON.=: _valueQuestionText
    , "requiredLevel" BSON.=: _valueQuestionRequiredLevel
    , "tagUuids" BSON.=: _valueQuestionTagUuids
    , "expertUuids" BSON.=: _valueQuestionExpertUuids
    , "referenceUuids" BSON.=: _valueQuestionReferenceUuids
    , "valueType" BSON.=: _valueQuestionValueType
    ]

instance FromBSON ValueQuestion

-- ------------------------------------------------
instance ToBSON IntegrationQuestion where
  toBSON IntegrationQuestion {..} =
    [ "questionType" BSON.=: "IntegrationQuestion"
    , "uuid" BSON.=: _integrationQuestionUuid
    , "title" BSON.=: _integrationQuestionTitle
    , "text" BSON.=: _integrationQuestionText
    , "requiredLevel" BSON.=: _integrationQuestionRequiredLevel
    , "tagUuids" BSON.=: _integrationQuestionTagUuids
    , "expertUuids" BSON.=: _integrationQuestionExpertUuids
    , "referenceUuids" BSON.=: _integrationQuestionReferenceUuids
    , "integrationUuid" BSON.=: _integrationQuestionIntegrationUuid
    , "props" BSON.=: _integrationQuestionProps
    ]

instance FromBSON IntegrationQuestion

-- -------------------------
-- ANSWER ------------------
-- -------------------------
instance ToBSON Answer

instance FromBSON Answer

-- -------------------------
-- EXPERT ------------------
-- -------------------------
instance ToBSON Expert

instance FromBSON Expert

-- -------------------------
-- REFERENCE ---------------
-- -------------------------
instance ToBSON Reference where
  toBSON (ResourcePageReference' event) = toBSON event
  toBSON (URLReference' event) = toBSON event
  toBSON (CrossReference' event) = toBSON event

instance FromBSON Reference where
  fromBSON doc = do
    referenceType <- BSON.lookup "referenceType" doc
    case referenceType of
      "ResourcePageReference" -> ResourcePageReference' <$> (fromBSON doc :: Maybe ResourcePageReference)
      "URLReference" -> URLReference' <$> (fromBSON doc :: Maybe URLReference)
      "CrossReference" -> CrossReference' <$> (fromBSON doc :: Maybe CrossReference)

-- ------------------------------------------------
instance ToBSON ResourcePageReference where
  toBSON ResourcePageReference {..} =
    [ "referenceType" BSON.=: "ResourcePageReference"
    , "uuid" BSON.=: _resourcePageReferenceUuid
    , "shortUuid" BSON.=: _resourcePageReferenceShortUuid
    ]

instance FromBSON ResourcePageReference

-- ------------------------------------------------
instance ToBSON URLReference where
  toBSON URLReference {..} =
    [ "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: _uRLReferenceUuid
    , "url" BSON.=: _uRLReferenceUrl
    , "label" BSON.=: _uRLReferenceLabel
    ]

instance FromBSON URLReference

-- ------------------------------------------------
instance ToBSON CrossReference where
  toBSON CrossReference {..} =
    [ "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: _crossReferenceUuid
    , "targetUuid" BSON.=: _crossReferenceTargetUuid
    , "description" BSON.=: _crossReferenceDescription
    ]

instance FromBSON CrossReference

-- -------------------------
-- METRIC ------------------
-- -------------------------
instance ToBSON Metric

instance FromBSON Metric

instance ToBSON MetricMeasure where
  toBSON MetricMeasure {..} =
    [ "metricUuid" BSON.=: _metricMeasureMetricUuid
    , "measure" BSON.=: _metricMeasureMeasure
    , "weight" BSON.=: _metricMeasureWeight
    ]

instance FromBSON MetricMeasure where
  fromBSON doc = do
    _metricMeasureMetricUuid <- BSON.lookup "metricUuid" doc
    _metricMeasureMeasure <- BSON.lookup "measure" doc
    _metricMeasureWeight <- BSON.lookup "weight" doc
    return MetricMeasure {..}

-- -------------------------
-- TAG ---------------------
-- -------------------------
instance ToBSON Tag

instance FromBSON Tag

-- -------------------------
-- INTEGRATION -------------
-- -------------------------
instance ToBSON Integration

instance FromBSON Integration
