module Api.Resource.KnowledgeModel.KnowledgeModelDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Common
import Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelDTO = KnowledgeModelDTO
  { _knowledgeModelDTOUuid :: U.UUID
  , _knowledgeModelDTOName :: String
  , _knowledgeModelDTOChapters :: [ChapterDTO]
  , _knowledgeModelDTOTags :: [TagDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data ChapterDTO = ChapterDTO
  { _chapterDTOUuid :: U.UUID
  , _chapterDTOTitle :: String
  , _chapterDTOText :: String
  , _chapterDTOQuestions :: [QuestionDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data QuestionDTO
  = OptionsQuestionDTO' OptionsQuestionDTO
  | ListQuestionDTO' ListQuestionDTO
  | ValueQuestionDTO' ValueQuestionDTO
  deriving (Show, Eq)

data OptionsQuestionDTO = OptionsQuestionDTO
  { _optionsQuestionDTOUuid :: U.UUID
  , _optionsQuestionDTOTitle :: String
  , _optionsQuestionDTOText :: Maybe String
  , _optionsQuestionDTORequiredLevel :: Maybe Int
  , _optionsQuestionDTOTagUuids :: [U.UUID]
  , _optionsQuestionDTOExperts :: [ExpertDTO]
  , _optionsQuestionDTOReferences :: [ReferenceDTO]
  , _optionsQuestionDTOAnswers :: [AnswerDTO]
  } deriving (Show, Eq)

data ListQuestionDTO = ListQuestionDTO
  { _listQuestionDTOUuid :: U.UUID
  , _listQuestionDTOTitle :: String
  , _listQuestionDTOText :: Maybe String
  , _listQuestionDTORequiredLevel :: Maybe Int
  , _listQuestionDTOTagUuids :: [U.UUID]
  , _listQuestionDTOExperts :: [ExpertDTO]
  , _listQuestionDTOReferences :: [ReferenceDTO]
  , _listQuestionDTOItemTemplateTitle :: String
  , _listQuestionDTOItemTemplateQuestions :: [QuestionDTO]
  } deriving (Show, Eq)

data ValueQuestionDTO = ValueQuestionDTO
  { _valueQuestionDTOUuid :: U.UUID
  , _valueQuestionDTOTitle :: String
  , _valueQuestionDTOText :: Maybe String
  , _valueQuestionDTORequiredLevel :: Maybe Int
  , _valueQuestionDTOTagUuids :: [U.UUID]
  , _valueQuestionDTOExperts :: [ExpertDTO]
  , _valueQuestionDTOReferences :: [ReferenceDTO]
  , _valueQuestionDTOValueType :: QuestionValueType
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data AnswerDTO = AnswerDTO
  { _answerDTOUuid :: U.UUID
  , _answerDTOLabel :: String
  , _answerDTOAdvice :: Maybe String
  , _answerDTOFollowUps :: [QuestionDTO]
  , _answerDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data ExpertDTO = ExpertDTO
  { _expertDTOUuid :: U.UUID
  , _expertDTOName :: String
  , _expertDTOEmail :: String
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data ReferenceDTO
  = ResourcePageReferenceDTO' ResourcePageReferenceDTO
  | URLReferenceDTO' URLReferenceDTO
  | CrossReferenceDTO' CrossReferenceDTO
  deriving (Show, Eq)

data ResourcePageReferenceDTO = ResourcePageReferenceDTO
  { _resourcePageReferenceDTOUuid :: U.UUID
  , _resourcePageReferenceDTOShortUuid :: String
  } deriving (Show, Eq)

data URLReferenceDTO = URLReferenceDTO
  { _uRLReferenceDTOUuid :: U.UUID
  , _uRLReferenceDTOUrl :: String
  , _uRLReferenceDTOLabel :: String
  } deriving (Show, Eq)

data CrossReferenceDTO = CrossReferenceDTO
  { _crossReferenceDTOUuid :: U.UUID
  , _crossReferenceDTOTargetUuid :: U.UUID
  , _crossReferenceDTODescription :: String
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data MetricDTO = MetricDTO
  { _metricDTOUuid :: U.UUID
  , _metricDTOTitle :: String
  , _metricDTOAbbreviation :: Maybe String
  , _metricDTODescription :: Maybe String
  , _metricDTOReferences :: [ReferenceDTO]
  , _metricDTOCreatedAt :: UTCTime
  , _metricDTOUpdatedAt :: UTCTime
  } deriving (Show)

instance Eq MetricDTO where
  a == b =
    _metricDTOUuid a == _metricDTOUuid b &&
    _metricDTOTitle a == _metricDTOTitle b &&
    _metricDTOAbbreviation a == _metricDTOAbbreviation b &&
    _metricDTODescription a == _metricDTODescription b && _metricDTOReferences a == _metricDTOReferences b

data MetricMeasureDTO = MetricMeasureDTO
  { _metricMeasureDTOMetricUuid :: U.UUID
  , _metricMeasureDTOMeasure :: Double
  , _metricMeasureDTOWeight :: Double
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data TagDTO = TagDTO
  { _tagDTOUuid :: U.UUID
  , _tagDTOName :: String
  , _tagDTODescription :: Maybe String
  , _tagDTOColor :: String
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModelDTO where
  toJSON KnowledgeModelDTO {..} =
    object
      [ "uuid" .= _knowledgeModelDTOUuid
      , "name" .= _knowledgeModelDTOName
      , "chapters" .= _knowledgeModelDTOChapters
      , "tags" .= _knowledgeModelDTOTags
      ]

-- --------------------------------------------------------------------
instance ToJSON ChapterDTO where
  toJSON ChapterDTO {..} =
    object
      [ "uuid" .= _chapterDTOUuid
      , "title" .= _chapterDTOTitle
      , "text" .= _chapterDTOText
      , "questions" .= _chapterDTOQuestions
      ]

-- --------------------------------------------------------------------
instance ToJSON QuestionDTO where
  toJSON (OptionsQuestionDTO' event) = toJSON event
  toJSON (ListQuestionDTO' event) = toJSON event
  toJSON (ValueQuestionDTO' event) = toJSON event

instance ToJSON OptionsQuestionDTO where
  toJSON OptionsQuestionDTO {..} =
    object
      [ "questionType" .= "OptionsQuestion"
      , "uuid" .= _optionsQuestionDTOUuid
      , "title" .= _optionsQuestionDTOTitle
      , "text" .= _optionsQuestionDTOText
      , "requiredLevel" .= _optionsQuestionDTORequiredLevel
      , "tagUuids" .= _optionsQuestionDTOTagUuids
      , "references" .= _optionsQuestionDTOReferences
      , "experts" .= _optionsQuestionDTOExperts
      , "answers" .= _optionsQuestionDTOAnswers
      ]

instance ToJSON ListQuestionDTO where
  toJSON ListQuestionDTO {..} =
    object
      [ "questionType" .= "ListQuestion"
      , "uuid" .= _listQuestionDTOUuid
      , "title" .= _listQuestionDTOTitle
      , "text" .= _listQuestionDTOText
      , "requiredLevel" .= _listQuestionDTORequiredLevel
      , "tagUuids" .= _listQuestionDTOTagUuids
      , "references" .= _listQuestionDTOReferences
      , "experts" .= _listQuestionDTOExperts
      , "itemTemplateTitle" .= _listQuestionDTOItemTemplateTitle
      , "itemTemplateQuestions" .= _listQuestionDTOItemTemplateQuestions
      ]

instance ToJSON ValueQuestionDTO where
  toJSON ValueQuestionDTO {..} =
    object
      [ "questionType" .= "ValueQuestion"
      , "uuid" .= _valueQuestionDTOUuid
      , "title" .= _valueQuestionDTOTitle
      , "text" .= _valueQuestionDTOText
      , "requiredLevel" .= _valueQuestionDTORequiredLevel
      , "tagUuids" .= _valueQuestionDTOTagUuids
      , "references" .= _valueQuestionDTOReferences
      , "experts" .= _valueQuestionDTOExperts
      , "valueType" .= serializeQuestionValueType _valueQuestionDTOValueType
      ]

-- --------------------------------------------------------------------
instance ToJSON AnswerDTO where
  toJSON AnswerDTO {..} =
    object
      [ "uuid" .= _answerDTOUuid
      , "label" .= _answerDTOLabel
      , "advice" .= _answerDTOAdvice
      , "followUps" .= _answerDTOFollowUps
      , "metricMeasures" .= _answerDTOMetricMeasures
      ]

-- --------------------------------------------------------------------
instance ToJSON ExpertDTO where
  toJSON ExpertDTO {..} = object ["uuid" .= _expertDTOUuid, "name" .= _expertDTOName, "email" .= _expertDTOEmail]

-- --------------------------------------------------------------------
instance ToJSON ReferenceDTO where
  toJSON (ResourcePageReferenceDTO' event) = toJSON event
  toJSON (URLReferenceDTO' event) = toJSON event
  toJSON (CrossReferenceDTO' event) = toJSON event

instance ToJSON ResourcePageReferenceDTO where
  toJSON ResourcePageReferenceDTO {..} =
    object
      [ "referenceType" .= "ResourcePageReference"
      , "uuid" .= _resourcePageReferenceDTOUuid
      , "shortUuid" .= _resourcePageReferenceDTOShortUuid
      ]

instance ToJSON URLReferenceDTO where
  toJSON URLReferenceDTO {..} =
    object
      [ "referenceType" .= "URLReference"
      , "uuid" .= _uRLReferenceDTOUuid
      , "url" .= _uRLReferenceDTOUrl
      , "label" .= _uRLReferenceDTOLabel
      ]

instance ToJSON CrossReferenceDTO where
  toJSON CrossReferenceDTO {..} =
    object
      [ "referenceType" .= "CrossReference"
      , "uuid" .= _crossReferenceDTOUuid
      , "targetUuid" .= _crossReferenceDTOTargetUuid
      , "description" .= _crossReferenceDTODescription
      ]

-- --------------------------------------------------------------------
instance ToJSON MetricDTO where
  toJSON MetricDTO {..} =
    object
      [ "uuid" .= _metricDTOUuid
      , "title" .= _metricDTOTitle
      , "abbreviation" .= _metricDTOAbbreviation
      , "description" .= _metricDTODescription
      , "references" .= _metricDTOReferences
      , "createdAt" .= _metricDTOCreatedAt
      , "updatedAt" .= _metricDTOUpdatedAt
      ]

instance ToJSON MetricMeasureDTO where
  toJSON MetricMeasureDTO {..} =
    object
      [ "metricUuid" .= _metricMeasureDTOMetricUuid
      , "measure" .= _metricMeasureDTOMeasure
      , "weight" .= _metricMeasureDTOWeight
      ]

-- --------------------------------------------------------------------
instance ToJSON TagDTO where
  toJSON TagDTO {..} =
    object ["uuid" .= _tagDTOUuid, "name" .= _tagDTOName, "description" .= _tagDTODescription, "color" .= _tagDTOColor]

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON KnowledgeModelDTO where
  parseJSON (Object o) = do
    _knowledgeModelDTOUuid <- o .: "uuid"
    _knowledgeModelDTOName <- o .: "name"
    _knowledgeModelDTOChapters <- o .: "chapters"
    _knowledgeModelDTOTags <- o .: "tags"
    return KnowledgeModelDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON ChapterDTO where
  parseJSON (Object o) = do
    _chapterDTOUuid <- o .: "uuid"
    _chapterDTOTitle <- o .: "title"
    _chapterDTOText <- o .: "text"
    _chapterDTOQuestions <- o .: "questions"
    return ChapterDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON QuestionDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (OptionsQuestionDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (ListQuestionDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (ValueQuestionDTO' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

instance FromJSON OptionsQuestionDTO where
  parseJSON (Object o) = do
    _optionsQuestionDTOUuid <- o .: "uuid"
    _optionsQuestionDTOTitle <- o .: "title"
    _optionsQuestionDTOText <- o .: "text"
    _optionsQuestionDTORequiredLevel <- o .: "requiredLevel"
    _optionsQuestionDTOTagUuids <- o .: "tagUuids"
    _optionsQuestionDTOExperts <- o .: "experts"
    _optionsQuestionDTOReferences <- o .: "references"
    _optionsQuestionDTOAnswers <- o .: "answers"
    return OptionsQuestionDTO {..}
  parseJSON _ = mzero

instance FromJSON ListQuestionDTO where
  parseJSON (Object o) = do
    _listQuestionDTOUuid <- o .: "uuid"
    _listQuestionDTOTitle <- o .: "title"
    _listQuestionDTOText <- o .: "text"
    _listQuestionDTORequiredLevel <- o .: "requiredLevel"
    _listQuestionDTOTagUuids <- o .: "tagUuids"
    _listQuestionDTOExperts <- o .: "experts"
    _listQuestionDTOReferences <- o .: "references"
    _listQuestionDTOItemTemplateTitle <- o .: "itemTemplateTitle"
    _listQuestionDTOItemTemplateQuestions <- o .: "itemTemplateQuestions"
    return ListQuestionDTO {..}
  parseJSON _ = mzero

instance FromJSON ValueQuestionDTO where
  parseJSON (Object o) = do
    _valueQuestionDTOUuid <- o .: "uuid"
    _valueQuestionDTOTitle <- o .: "title"
    _valueQuestionDTOText <- o .: "text"
    _valueQuestionDTORequiredLevel <- o .: "requiredLevel"
    _valueQuestionDTOTagUuids <- o .: "tagUuids"
    _valueQuestionDTOExperts <- o .: "experts"
    _valueQuestionDTOReferences <- o .: "references"
    valueType <- o .: "valueType"
    case deserializeQuestionValueType valueType of
      (Just _valueQuestionDTOValueType) -> return ValueQuestionDTO {..}
      Nothing -> fail "Unsupported question value type"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON AnswerDTO where
  parseJSON (Object o) = do
    _answerDTOUuid <- o .: "uuid"
    _answerDTOLabel <- o .: "label"
    _answerDTOAdvice <- o .: "advice"
    _answerDTOFollowUps <- o .: "followUps"
    _answerDTOMetricMeasures <- o .: "metricMeasures"
    return AnswerDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON ExpertDTO where
  parseJSON (Object o) = do
    _expertDTOUuid <- o .: "uuid"
    _expertDTOName <- o .: "name"
    _expertDTOEmail <- o .: "email"
    return ExpertDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON ReferenceDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (ResourcePageReferenceDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (URLReferenceDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (CrossReferenceDTO' event)
      _ -> fail "One of the references has unsupported referenceType"
  parseJSON _ = mzero

instance FromJSON ResourcePageReferenceDTO where
  parseJSON (Object o) = do
    _resourcePageReferenceDTOUuid <- o .: "uuid"
    _resourcePageReferenceDTOShortUuid <- o .: "shortUuid"
    return ResourcePageReferenceDTO {..}
  parseJSON _ = mzero

instance FromJSON URLReferenceDTO where
  parseJSON (Object o) = do
    _uRLReferenceDTOUuid <- o .: "uuid"
    _uRLReferenceDTOUrl <- o .: "url"
    _uRLReferenceDTOLabel <- o .: "label"
    return URLReferenceDTO {..}
  parseJSON _ = mzero

instance FromJSON CrossReferenceDTO where
  parseJSON (Object o) = do
    _crossReferenceDTOUuid <- o .: "uuid"
    _crossReferenceDTOTargetUuid <- o .: "targetUuid"
    _crossReferenceDTODescription <- o .: "description"
    return CrossReferenceDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON MetricDTO where
  parseJSON (Object o) = do
    _metricDTOUuid <- o .: "uuid"
    _metricDTOTitle <- o .: "title"
    _metricDTOAbbreviation <- o .: "abbreviation"
    _metricDTODescription <- o .: "description"
    _metricDTOReferences <- o .: "references"
    _metricDTOCreatedAt <- o .: "updatedAt"
    _metricDTOUpdatedAt <- o .: "createdAt"
    return MetricDTO {..}
  parseJSON _ = mzero

instance FromJSON MetricMeasureDTO where
  parseJSON (Object o) = do
    _metricMeasureDTOMetricUuid <- o .: "metricUuid"
    _metricMeasureDTOMeasure <- o .: "measure"
    _metricMeasureDTOWeight <- o .: "weight"
    return MetricMeasureDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON TagDTO where
  parseJSON (Object o) = do
    _tagDTOUuid <- o .: "uuid"
    _tagDTOName <- o .: "name"
    _tagDTODescription <- o .: "description"
    _tagDTOColor <- o .: "color"
    return TagDTO {..}
  parseJSON _ = mzero
