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
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data ChapterDTO = ChapterDTO
  { _chapterDTOUuid :: U.UUID
  , _chapterDTOTitle :: String
  , _chapterDTOText :: String
  , _chapterDTOQuestions :: [QuestionDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data QuestionDTO = QuestionDTO
  { _questionDTOUuid :: U.UUID
  , _questionDTOQType :: QuestionType
  , _questionDTOTitle :: String
  , _questionDTOText :: String
  , _questionDTORequiredLevel :: Maybe Int
  , _questionDTOAnswers :: Maybe [AnswerDTO]
  , _questionDTOAnswerItemTemplate :: Maybe AnswerItemTemplateDTO
  , _questionDTOReferences :: [ReferenceDTO]
  , _questionDTOExperts :: [ExpertDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data AnswerDTO = AnswerDTO
  { _answerDTOUuid :: U.UUID
  , _answerDTOLabel :: String
  , _answerDTOAdvice :: Maybe String
  , _answerDTOFollowUps :: [QuestionDTO]
  , _answerDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq)

data AnswerItemTemplateDTO = AnswerItemTemplateDTO
  { _answerItemTemplateDTOTitle :: String
  , _answerItemTemplateDTOQuestions :: [QuestionDTO]
  } deriving (Show, Eq)

data AnswerItemTemplatePlainDTO = AnswerItemTemplatePlainDTO
  { _answerItemTemplatePlainDTOTitle :: String
  } deriving (Show, Eq)

data AnswerItemTemplatePlainWithIdsDTO = AnswerItemTemplatePlainWithIdsDTO
  { _answerItemTemplatePlainWithIdsDTOTitle :: String
  , _answerItemTemplatePlainWithIdsDTOQuestionIds :: [U.UUID]
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
  , _uRLReferenceDTOAnchor :: String
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
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModelDTO where
  toJSON KnowledgeModelDTO {..} =
    object
      ["uuid" .= _knowledgeModelDTOUuid, "name" .= _knowledgeModelDTOName, "chapters" .= _knowledgeModelDTOChapters]

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
  toJSON QuestionDTO {..} =
    object
      [ "uuid" .= _questionDTOUuid
      , "type" .= serializeQuestionType _questionDTOQType
      , "title" .= _questionDTOTitle
      , "text" .= _questionDTOText
      , "requiredLevel" .= _questionDTORequiredLevel
      , "answers" .= _questionDTOAnswers
      , "answerItemTemplate" .= _questionDTOAnswerItemTemplate
      , "references" .= _questionDTOReferences
      , "experts" .= _questionDTOExperts
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

instance ToJSON AnswerItemTemplateDTO where
  toJSON AnswerItemTemplateDTO {..} =
    object ["title" .= _answerItemTemplateDTOTitle, "questions" .= _answerItemTemplateDTOQuestions]

instance ToJSON AnswerItemTemplatePlainDTO where
  toJSON AnswerItemTemplatePlainDTO {..} = object ["title" .= _answerItemTemplatePlainDTOTitle]

instance ToJSON AnswerItemTemplatePlainWithIdsDTO where
  toJSON AnswerItemTemplatePlainWithIdsDTO {..} =
    object
      [ "title" .= _answerItemTemplatePlainWithIdsDTOTitle
      , "questionIds" .= _answerItemTemplatePlainWithIdsDTOQuestionIds
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
      , "anchor" .= _uRLReferenceDTOAnchor
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
-- --------------------------------------------------------------------
instance FromJSON KnowledgeModelDTO where
  parseJSON (Object o) = do
    _knowledgeModelDTOUuid <- o .: "uuid"
    _knowledgeModelDTOName <- o .: "name"
    _knowledgeModelDTOChapters <- o .: "chapters"
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
    _questionDTOUuid <- o .: "uuid"
    _questionDTOTitle <- o .: "title"
    _questionDTOText <- o .: "text"
    _questionDTORequiredLevel <- o .: "requiredLevel"
    _questionDTOAnswers <- o .: "answers"
    _questionDTOAnswerItemTemplate <- o .: "answerItemTemplate"
    _questionDTOExperts <- o .: "experts"
    _questionDTOReferences <- o .: "answers"
    questionType <- o .: "type"
    case deserializeQuestionType questionType of
      (Just _questionDTOQType) -> return QuestionDTO {..}
      Nothing -> fail "Unsupported question type"
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

instance FromJSON AnswerItemTemplateDTO where
  parseJSON (Object o) = do
    _answerItemTemplateDTOTitle <- o .: "title"
    _answerItemTemplateDTOQuestions <- o .: "questions"
    return AnswerItemTemplateDTO {..}
  parseJSON _ = mzero

instance FromJSON AnswerItemTemplatePlainDTO where
  parseJSON (Object o) = do
    _answerItemTemplatePlainDTOTitle <- o .: "title"
    return AnswerItemTemplatePlainDTO {..}
  parseJSON _ = mzero

instance FromJSON AnswerItemTemplatePlainWithIdsDTO where
  parseJSON (Object o) = do
    _answerItemTemplatePlainWithIdsDTOTitle <- o .: "title"
    _answerItemTemplatePlainWithIdsDTOQuestionIds <- o .: "questionIds"
    return AnswerItemTemplatePlainWithIdsDTO {..}
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
    _uRLReferenceDTOAnchor <- o .: "anchor"
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
