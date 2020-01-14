module Wizard.Database.BSON.KnowledgeModel.KnowledgeModel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.BSON.Common ()

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel where
  toBSON KnowledgeModel {..} =
    [ "uuid" BSON.=: _knowledgeModelUuid
    , "name" BSON.=: _knowledgeModelName
    , "chapterUuids" BSON.=: _knowledgeModelChapterUuids
    , "tagUuids" BSON.=: _knowledgeModelTagUuids
    , "integrationUuids" BSON.=: _knowledgeModelIntegrationUuids
    , "entities" BSON.=: _knowledgeModelEntities
    ]

instance FromBSON KnowledgeModel where
  fromBSON doc = do
    _knowledgeModelUuid <- BSON.lookup "uuid" doc
    _knowledgeModelName <- BSON.lookup "name" doc
    _knowledgeModelChapterUuids <- BSON.lookup "chapterUuids" doc
    _knowledgeModelTagUuids <- BSON.lookup "tagUuids" doc
    _knowledgeModelIntegrationUuids <- BSON.lookup "integrationUuids" doc
    _knowledgeModelEntities <- BSON.lookup "entities" doc
    return KnowledgeModel {..}

instance ToBSON KnowledgeModelEntities where
  toBSON KnowledgeModelEntities {..} =
    [ "chapters" BSON.=: _knowledgeModelEntitiesChapters
    , "questions" BSON.=: _knowledgeModelEntitiesQuestions
    , "answers" BSON.=: _knowledgeModelEntitiesAnswers
    , "experts" BSON.=: _knowledgeModelEntitiesExperts
    , "references" BSON.=: _knowledgeModelEntitiesReferences
    , "integrations" BSON.=: _knowledgeModelEntitiesIntegrations
    , "tags" BSON.=: _knowledgeModelEntitiesTags
    ]

instance FromBSON KnowledgeModelEntities where
  fromBSON doc = do
    _knowledgeModelEntitiesChapters <- BSON.lookup "chapters" doc
    _knowledgeModelEntitiesQuestions <- BSON.lookup "questions" doc
    _knowledgeModelEntitiesAnswers <- BSON.lookup "answers" doc
    _knowledgeModelEntitiesExperts <- BSON.lookup "experts" doc
    _knowledgeModelEntitiesReferences <- BSON.lookup "references" doc
    _knowledgeModelEntitiesIntegrations <- BSON.lookup "integrations" doc
    _knowledgeModelEntitiesTags <- BSON.lookup "tags" doc
    return KnowledgeModelEntities {..}

-- -------------------------
-- CHAPTER -----------------
-- -------------------------
instance ToBSON Chapter where
  toBSON Chapter {..} =
    [ "uuid" BSON.=: _chapterUuid
    , "title" BSON.=: _chapterTitle
    , "text" BSON.=: _chapterText
    , "questionUuids" BSON.=: _chapterQuestionUuids
    ]

instance FromBSON Chapter where
  fromBSON doc = do
    _chapterUuid <- BSON.lookup "uuid" doc
    _chapterTitle <- BSON.lookup "title" doc
    _chapterText <- BSON.lookup "text" doc
    _chapterQuestionUuids <- BSON.lookup "questionUuids" doc
    return Chapter {..}

-- -------------------------
-- QUESTION ----------------
-- -------------------------
instance ToBSON Question where
  toBSON (OptionsQuestion' event) = toBSON event
  toBSON (ListQuestion' event) = toBSON event
  toBSON (ValueQuestion' event) = toBSON event

instance FromBSON Question where
  fromBSON doc = do
    questionType <- BSON.lookup "questionType" doc
    case questionType of
      "OptionsQuestion" -> OptionsQuestion' <$> (fromBSON doc :: Maybe OptionsQuestion)
      "ListQuestion" -> ListQuestion' <$> (fromBSON doc :: Maybe ListQuestion)
      "ValueQuestion" -> ValueQuestion' <$> (fromBSON doc :: Maybe ValueQuestion)

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

instance FromBSON OptionsQuestion where
  fromBSON doc = do
    _optionsQuestionUuid <- BSON.lookup "uuid" doc
    _optionsQuestionTitle <- BSON.lookup "title" doc
    _optionsQuestionText <- BSON.lookup "text" doc
    _optionsQuestionRequiredLevel <- BSON.lookup "requiredLevel" doc
    _optionsQuestionTagUuids <- BSON.lookup "tagUuids" doc
    _optionsQuestionExpertUuids <- BSON.lookup "expertUuids" doc
    _optionsQuestionReferenceUuids <- BSON.lookup "referenceUuids" doc
    _optionsQuestionAnswerUuids <- BSON.lookup "answerUuids" doc
    return OptionsQuestion {..}

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

instance FromBSON ListQuestion where
  fromBSON doc = do
    _listQuestionUuid <- BSON.lookup "uuid" doc
    _listQuestionTitle <- BSON.lookup "title" doc
    _listQuestionText <- BSON.lookup "text" doc
    _listQuestionRequiredLevel <- BSON.lookup "requiredLevel" doc
    _listQuestionTagUuids <- BSON.lookup "tagUuids" doc
    _listQuestionExpertUuids <- BSON.lookup "expertUuids" doc
    _listQuestionReferenceUuids <- BSON.lookup "referenceUuids" doc
    _listQuestionItemTemplateQuestionUuids <- BSON.lookup "itemTemplateQuestionUuids" doc
    return ListQuestion {..}

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

instance FromBSON ValueQuestion where
  fromBSON doc = do
    _valueQuestionUuid <- BSON.lookup "uuid" doc
    _valueQuestionTitle <- BSON.lookup "title" doc
    _valueQuestionText <- BSON.lookup "text" doc
    _valueQuestionRequiredLevel <- BSON.lookup "requiredLevel" doc
    _valueQuestionTagUuids <- BSON.lookup "tagUuids" doc
    _valueQuestionExpertUuids <- BSON.lookup "expertUuids" doc
    _valueQuestionReferenceUuids <- BSON.lookup "referenceUuids" doc
    _valueQuestionValueType <- BSON.lookup "valueType" doc
    return ValueQuestion {..}

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

instance FromBSON IntegrationQuestion where
  fromBSON doc = do
    _integrationQuestionUuid <- BSON.lookup "uuid" doc
    _integrationQuestionTitle <- BSON.lookup "title" doc
    _integrationQuestionText <- BSON.lookup "text" doc
    _integrationQuestionRequiredLevel <- BSON.lookup "requiredLevel" doc
    _integrationQuestionTagUuids <- BSON.lookup "tagUuids" doc
    _integrationQuestionExpertUuids <- BSON.lookup "expertUuids" doc
    _integrationQuestionReferenceUuids <- BSON.lookup "referenceUuids" doc
    _integrationQuestionIntegrationUuid <- BSON.lookup "integrationUuid" doc
    _integrationQuestionProps <- BSON.lookup "valueType" doc
    return IntegrationQuestion {..}

-- -------------------------
-- ANSWER ------------------
-- -------------------------
instance ToBSON Answer where
  toBSON Answer {..} =
    [ "uuid" BSON.=: _answerUuid
    , "label" BSON.=: _answerLabel
    , "advice" BSON.=: _answerAdvice
    , "followUpUuids" BSON.=: _answerFollowUpUuids
    , "metricMeasures" BSON.=: _answerMetricMeasures
    ]

instance FromBSON Answer where
  fromBSON doc = do
    _answerUuid <- BSON.lookup "uuid" doc
    _answerLabel <- BSON.lookup "label" doc
    _answerAdvice <- BSON.lookup "advice" doc
    _answerFollowUpUuids <- BSON.lookup "followUpUuids" doc
    _answerMetricMeasures <- BSON.lookup "metricMeasures" doc
    return Answer {..}

-- -------------------------
-- EXPERT ------------------
-- -------------------------
instance ToBSON Expert where
  toBSON Expert {..} = ["uuid" BSON.=: _expertUuid, "name" BSON.=: _expertName, "email" BSON.=: _expertEmail]

instance FromBSON Expert where
  fromBSON doc = do
    _expertUuid <- BSON.lookup "uuid" doc
    _expertName <- BSON.lookup "name" doc
    _expertEmail <- BSON.lookup "email" doc
    return Expert {..}

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

instance FromBSON ResourcePageReference where
  fromBSON doc = do
    _resourcePageReferenceUuid <- BSON.lookup "uuid" doc
    _resourcePageReferenceShortUuid <- BSON.lookup "shortUuid" doc
    return ResourcePageReference {..}

-- ------------------------------------------------
instance ToBSON URLReference where
  toBSON URLReference {..} =
    [ "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: _uRLReferenceUuid
    , "url" BSON.=: _uRLReferenceUrl
    , "label" BSON.=: _uRLReferenceLabel
    ]

instance FromBSON URLReference where
  fromBSON doc = do
    _uRLReferenceUuid <- BSON.lookup "uuid" doc
    _uRLReferenceUrl <- BSON.lookup "url" doc
    _uRLReferenceLabel <- BSON.lookup "label" doc
    return URLReference {..}

-- ------------------------------------------------
instance ToBSON CrossReference where
  toBSON CrossReference {..} =
    [ "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: _crossReferenceUuid
    , "targetUuid" BSON.=: _crossReferenceTargetUuid
    , "description" BSON.=: _crossReferenceDescription
    ]

instance FromBSON CrossReference where
  fromBSON doc = do
    _crossReferenceUuid <- BSON.lookup "uuid" doc
    _crossReferenceTargetUuid <- BSON.lookup "targetUuid" doc
    _crossReferenceDescription <- BSON.lookup "description" doc
    return CrossReference {..}

-- -------------------------
-- METRIC ------------------
-- -------------------------
instance ToBSON Metric where
  toBSON Metric {..} =
    [ "uuid" BSON.=: _metricUuid
    , "title" BSON.=: _metricTitle
    , "abbreviation" BSON.=: _metricAbbreviation
    , "description" BSON.=: _metricDescription
    , "references" BSON.=: _metricReferences
    , "createdAt" BSON.=: _metricCreatedAt
    , "updatedAt" BSON.=: _metricUpdatedAt
    ]

instance FromBSON Metric where
  fromBSON doc = do
    _metricUuid <- BSON.lookup "uuid" doc
    _metricTitle <- BSON.lookup "title" doc
    _metricAbbreviation <- BSON.lookup "abbreviation" doc
    _metricDescription <- BSON.lookup "description" doc
    _metricReferences <- BSON.lookup "references" doc
    _metricCreatedAt <- BSON.lookup "createdAt" doc
    _metricUpdatedAt <- BSON.lookup "updatedAt" doc
    return Metric {..}

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
instance ToBSON Tag where
  toBSON Tag {..} =
    ["uuid" BSON.=: _tagUuid, "name" BSON.=: _tagName, "description" BSON.=: _tagDescription, "color" BSON.=: _tagColor]

instance FromBSON Tag where
  fromBSON doc = do
    _tagUuid <- BSON.lookup "uuid" doc
    _tagName <- BSON.lookup "name" doc
    _tagDescription <- BSON.lookup "description" doc
    _tagColor <- BSON.lookup "color" doc
    return Tag {..}

-- -------------------------
-- INTEGRATION -------------
-- -------------------------
instance ToBSON Integration where
  toBSON Integration {..} =
    [ "uuid" BSON.=: _integrationUuid
    , "id" BSON.=: _integrationIId
    , "name" BSON.=: _integrationName
    , "props" BSON.=: _integrationProps
    , "logo" BSON.=: _integrationLogo
    , "requestMethod" BSON.=: _integrationRequestMethod
    , "requestUrl" BSON.=: _integrationRequestUrl
    , "requestHeaders" BSON.=: _integrationRequestHeaders
    , "requestBody" BSON.=: _integrationRequestBody
    , "responseListField" BSON.=: _integrationResponseListField
    , "responseIdField" BSON.=: _integrationResponseIdField
    , "responseNameField" BSON.=: _integrationResponseNameField
    , "itemUrl" BSON.=: _integrationItemUrl
    ]

instance FromBSON Integration where
  fromBSON doc = do
    _integrationUuid <- BSON.lookup "uuid" doc
    _integrationIId <- BSON.lookup "id" doc
    _integrationName <- BSON.lookup "name" doc
    _integrationProps <- BSON.lookup "props" doc
    _integrationLogo <- BSON.lookup "logo" doc
    _integrationRequestMethod <- BSON.lookup "requestMethod" doc
    _integrationRequestUrl <- BSON.lookup "requestUrl" doc
    _integrationRequestHeaders <- BSON.lookup "requestHeaders" doc
    _integrationRequestBody <- BSON.lookup "requestBody" doc
    _integrationResponseListField <- BSON.lookup "responseListField" doc
    _integrationResponseIdField <- BSON.lookup "responseIdField" doc
    _integrationResponseNameField <- BSON.lookup "responseNameField" doc
    _integrationItemUrl <- BSON.lookup "itemUrl" doc
    return Integration {..}
