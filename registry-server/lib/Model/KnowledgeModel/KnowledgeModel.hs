module Model.KnowledgeModel.KnowledgeModel where

import Data.Map
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModel = KnowledgeModel
  { _knowledgeModelUuid :: U.UUID
  , _knowledgeModelName :: String
  , _knowledgeModelChapterUuids :: [U.UUID]
  , _knowledgeModelTagUuids :: [U.UUID]
  , _knowledgeModelIntegrationUuids :: [U.UUID]
  , _knowledgeModelEntities :: KnowledgeModelEntities
  } deriving (Show, Eq, Generic)

data KnowledgeModelEntities = KnowledgeModelEntities
  { _knowledgeModelEntitiesChapters :: Map U.UUID Chapter
  , _knowledgeModelEntitiesQuestions :: Map U.UUID Question
  , _knowledgeModelEntitiesAnswers :: Map U.UUID Answer
  , _knowledgeModelEntitiesExperts :: Map U.UUID Expert
  , _knowledgeModelEntitiesReferences :: Map U.UUID Reference
  , _knowledgeModelEntitiesIntegrations :: Map U.UUID Integration
  , _knowledgeModelEntitiesTags :: Map U.UUID Tag
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Chapter = Chapter
  { _chapterUuid :: U.UUID
  , _chapterTitle :: String
  , _chapterText :: Maybe String
  , _chapterQuestionUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | TextQuestionValueType
  deriving (Show, Eq, Generic)

data Question
  = OptionsQuestion' OptionsQuestion
  | ListQuestion' ListQuestion
  | ValueQuestion' ValueQuestion
  | IntegrationQuestion' IntegrationQuestion
  deriving (Show, Eq, Generic)

data OptionsQuestion = OptionsQuestion
  { _optionsQuestionUuid :: U.UUID
  , _optionsQuestionTitle :: String
  , _optionsQuestionText :: Maybe String
  , _optionsQuestionRequiredLevel :: Maybe Int
  , _optionsQuestionTagUuids :: [U.UUID]
  , _optionsQuestionExpertUuids :: [U.UUID]
  , _optionsQuestionReferenceUuids :: [U.UUID]
  , _optionsQuestionAnswerUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

data ListQuestion = ListQuestion
  { _listQuestionUuid :: U.UUID
  , _listQuestionTitle :: String
  , _listQuestionText :: Maybe String
  , _listQuestionRequiredLevel :: Maybe Int
  , _listQuestionTagUuids :: [U.UUID]
  , _listQuestionExpertUuids :: [U.UUID]
  , _listQuestionReferenceUuids :: [U.UUID]
  , _listQuestionItemTemplateQuestionUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)

data ValueQuestion = ValueQuestion
  { _valueQuestionUuid :: U.UUID
  , _valueQuestionTitle :: String
  , _valueQuestionText :: Maybe String
  , _valueQuestionRequiredLevel :: Maybe Int
  , _valueQuestionTagUuids :: [U.UUID]
  , _valueQuestionExpertUuids :: [U.UUID]
  , _valueQuestionReferenceUuids :: [U.UUID]
  , _valueQuestionValueType :: QuestionValueType
  } deriving (Show, Eq, Generic)

data IntegrationQuestion = IntegrationQuestion
  { _integrationQuestionUuid :: U.UUID
  , _integrationQuestionTitle :: String
  , _integrationQuestionText :: Maybe String
  , _integrationQuestionRequiredLevel :: Maybe Int
  , _integrationQuestionTagUuids :: [U.UUID]
  , _integrationQuestionExpertUuids :: [U.UUID]
  , _integrationQuestionReferenceUuids :: [U.UUID]
  , _integrationQuestionIntegrationUuid :: U.UUID
  , _integrationQuestionProps :: Map String String
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Answer = Answer
  { _answerUuid :: U.UUID
  , _answerLabel :: String
  , _answerAdvice :: Maybe String
  , _answerFollowUpUuids :: [U.UUID]
  , _answerMetricMeasures :: [MetricMeasure]
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Expert = Expert
  { _expertUuid :: U.UUID
  , _expertName :: String
  , _expertEmail :: String
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Reference
  = ResourcePageReference' ResourcePageReference
  | URLReference' URLReference
  | CrossReference' CrossReference
  deriving (Show, Eq, Generic)

data ResourcePageReference = ResourcePageReference
  { _resourcePageReferenceUuid :: U.UUID
  , _resourcePageReferenceShortUuid :: String
  } deriving (Show, Eq, Generic)

data URLReference = URLReference
  { _uRLReferenceUuid :: U.UUID
  , _uRLReferenceUrl :: String
  , _uRLReferenceLabel :: String
  } deriving (Show, Eq, Generic)

data CrossReference = CrossReference
  { _crossReferenceUuid :: U.UUID
  , _crossReferenceTargetUuid :: U.UUID
  , _crossReferenceDescription :: String
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Metric = Metric
  { _metricUuid :: U.UUID
  , _metricTitle :: String
  , _metricAbbreviation :: Maybe String
  , _metricDescription :: Maybe String
  , _metricReferences :: [Reference]
  , _metricCreatedAt :: UTCTime
  , _metricUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq Metric where
  a == b =
    _metricUuid a == _metricUuid b &&
    _metricTitle a == _metricTitle b &&
    _metricAbbreviation a == _metricAbbreviation b &&
    _metricDescription a == _metricDescription b && _metricReferences a == _metricReferences b

data MetricMeasure = MetricMeasure
  { _metricMeasureMetricUuid :: U.UUID
  , _metricMeasureMeasure :: Double
  , _metricMeasureWeight :: Double
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Tag = Tag
  { _tagUuid :: U.UUID
  , _tagName :: String
  , _tagDescription :: Maybe String
  , _tagColor :: String
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Integration = Integration
  { _integrationUuid :: U.UUID
  , _integrationIId :: String
  , _integrationName :: String
  , _integrationProps :: [String]
  , _integrationLogo :: String
  , _integrationRequestMethod :: String
  , _integrationRequestUrl :: String
  , _integrationRequestHeaders :: Map String String
  , _integrationRequestBody :: String
  , _integrationResponseListField :: String
  , _integrationResponseIdField :: String
  , _integrationResponseNameField :: String
  , _integrationItemUrl :: String
  } deriving (Show, Eq, Generic)
