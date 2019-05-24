module Api.Resource.KnowledgeModel.KnowledgeModelDTO where

import Data.Map
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelDTO = KnowledgeModelDTO
  { _knowledgeModelDTOUuid :: U.UUID
  , _knowledgeModelDTOName :: String
  , _knowledgeModelDTOChapters :: [ChapterDTO]
  , _knowledgeModelDTOTags :: [TagDTO]
  , _knowledgeModelDTOIntegrations :: [IntegrationDTO]
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data ChapterDTO = ChapterDTO
  { _chapterDTOUuid :: U.UUID
  , _chapterDTOTitle :: String
  , _chapterDTOText :: String
  , _chapterDTOQuestions :: [QuestionDTO]
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data QuestionDTO
  = OptionsQuestionDTO' OptionsQuestionDTO
  | ListQuestionDTO' ListQuestionDTO
  | ValueQuestionDTO' ValueQuestionDTO
  | IntegrationQuestionDTO' IntegrationQuestionDTO
  deriving (Show, Eq, Generic)

data OptionsQuestionDTO = OptionsQuestionDTO
  { _optionsQuestionDTOUuid :: U.UUID
  , _optionsQuestionDTOTitle :: String
  , _optionsQuestionDTOText :: Maybe String
  , _optionsQuestionDTORequiredLevel :: Maybe Int
  , _optionsQuestionDTOTagUuids :: [U.UUID]
  , _optionsQuestionDTOExperts :: [ExpertDTO]
  , _optionsQuestionDTOReferences :: [ReferenceDTO]
  , _optionsQuestionDTOAnswers :: [AnswerDTO]
  } deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

data ValueQuestionDTO = ValueQuestionDTO
  { _valueQuestionDTOUuid :: U.UUID
  , _valueQuestionDTOTitle :: String
  , _valueQuestionDTOText :: Maybe String
  , _valueQuestionDTORequiredLevel :: Maybe Int
  , _valueQuestionDTOTagUuids :: [U.UUID]
  , _valueQuestionDTOExperts :: [ExpertDTO]
  , _valueQuestionDTOReferences :: [ReferenceDTO]
  , _valueQuestionDTOValueType :: QuestionValueType
  } deriving (Show, Eq, Generic)

data IntegrationQuestionDTO = IntegrationQuestionDTO
  { _integrationQuestionDTOUuid :: U.UUID
  , _integrationQuestionDTOTitle :: String
  , _integrationQuestionDTOText :: Maybe String
  , _integrationQuestionDTORequiredLevel :: Maybe Int
  , _integrationQuestionDTOTagUuids :: [U.UUID]
  , _integrationQuestionDTOExperts :: [ExpertDTO]
  , _integrationQuestionDTOReferences :: [ReferenceDTO]
  , _integrationQuestionDTOIntegrationUuid :: U.UUID
  , _integrationQuestionDTOProps :: Map String String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data AnswerDTO = AnswerDTO
  { _answerDTOUuid :: U.UUID
  , _answerDTOLabel :: String
  , _answerDTOAdvice :: Maybe String
  , _answerDTOFollowUps :: [QuestionDTO]
  , _answerDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data ExpertDTO = ExpertDTO
  { _expertDTOUuid :: U.UUID
  , _expertDTOName :: String
  , _expertDTOEmail :: String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data ReferenceDTO
  = ResourcePageReferenceDTO' ResourcePageReferenceDTO
  | URLReferenceDTO' URLReferenceDTO
  | CrossReferenceDTO' CrossReferenceDTO
  deriving (Show, Eq, Generic)

data ResourcePageReferenceDTO = ResourcePageReferenceDTO
  { _resourcePageReferenceDTOUuid :: U.UUID
  , _resourcePageReferenceDTOShortUuid :: String
  } deriving (Show, Eq, Generic)

data URLReferenceDTO = URLReferenceDTO
  { _uRLReferenceDTOUuid :: U.UUID
  , _uRLReferenceDTOUrl :: String
  , _uRLReferenceDTOLabel :: String
  } deriving (Show, Eq, Generic)

data CrossReferenceDTO = CrossReferenceDTO
  { _crossReferenceDTOUuid :: U.UUID
  , _crossReferenceDTOTargetUuid :: U.UUID
  , _crossReferenceDTODescription :: String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data MetricDTO = MetricDTO
  { _metricDTOUuid :: U.UUID
  , _metricDTOTitle :: String
  , _metricDTOAbbreviation :: Maybe String
  , _metricDTODescription :: Maybe String
  , _metricDTOReferences :: [ReferenceDTO]
  , _metricDTOCreatedAt :: UTCTime
  , _metricDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

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
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data TagDTO = TagDTO
  { _tagDTOUuid :: U.UUID
  , _tagDTOName :: String
  , _tagDTODescription :: Maybe String
  , _tagDTOColor :: String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data IntegrationDTO = IntegrationDTO
  { _integrationDTOUuid :: U.UUID
  , _integrationDTOIId :: String
  , _integrationDTOName :: String
  , _integrationDTOProps :: [String]
  , _integrationDTOLogo :: String
  , _integrationDTORequestMethod :: String
  , _integrationDTORequestUrl :: String
  , _integrationDTORequestHeaders :: Map String String
  , _integrationDTORequestBody :: String
  , _integrationDTOResponseListField :: String
  , _integrationDTOResponseIdField :: String
  , _integrationDTOResponseNameField :: String
  , _integrationDTOItemUrl :: String
  } deriving (Show, Eq, Generic)
