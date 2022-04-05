module Shared.Model.KnowledgeModel.KnowledgeModel where

import Data.Map
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry

type KMParentMap = M.Map U.UUID U.UUID

data KnowledgeModel =
  KnowledgeModel
    { _knowledgeModelUuid :: U.UUID
    , _knowledgeModelAnnotations :: [MapEntry String String]
    , _knowledgeModelChapterUuids :: [U.UUID]
    , _knowledgeModelTagUuids :: [U.UUID]
    , _knowledgeModelIntegrationUuids :: [U.UUID]
    , _knowledgeModelMetricUuids :: [U.UUID]
    , _knowledgeModelPhaseUuids :: [U.UUID]
    , _knowledgeModelEntities :: KnowledgeModelEntities
    }
  deriving (Show, Eq, Generic)

data KnowledgeModelEntities =
  KnowledgeModelEntities
    { _knowledgeModelEntitiesChapters :: Map U.UUID Chapter
    , _knowledgeModelEntitiesQuestions :: Map U.UUID Question
    , _knowledgeModelEntitiesAnswers :: Map U.UUID Answer
    , _knowledgeModelEntitiesChoices :: Map U.UUID Choice
    , _knowledgeModelEntitiesExperts :: Map U.UUID Expert
    , _knowledgeModelEntitiesReferences :: Map U.UUID Reference
    , _knowledgeModelEntitiesIntegrations :: Map U.UUID Integration
    , _knowledgeModelEntitiesTags :: Map U.UUID Tag
    , _knowledgeModelEntitiesMetrics :: Map U.UUID Metric
    , _knowledgeModelEntitiesPhases :: Map U.UUID Phase
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Chapter =
  Chapter
    { _chapterUuid :: U.UUID
    , _chapterTitle :: String
    , _chapterText :: Maybe String
    , _chapterAnnotations :: [MapEntry String String]
    , _chapterQuestionUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | TextQuestionValueType
  deriving (Show, Eq, Generic, Read)

data Question
  = OptionsQuestion' OptionsQuestion
  | MultiChoiceQuestion' MultiChoiceQuestion
  | ListQuestion' ListQuestion
  | ValueQuestion' ValueQuestion
  | IntegrationQuestion' IntegrationQuestion
  deriving (Show, Eq, Generic)

data OptionsQuestion =
  OptionsQuestion
    { _optionsQuestionUuid :: U.UUID
    , _optionsQuestionTitle :: String
    , _optionsQuestionText :: Maybe String
    , _optionsQuestionRequiredPhaseUuid :: Maybe U.UUID
    , _optionsQuestionAnnotations :: [MapEntry String String]
    , _optionsQuestionTagUuids :: [U.UUID]
    , _optionsQuestionExpertUuids :: [U.UUID]
    , _optionsQuestionReferenceUuids :: [U.UUID]
    , _optionsQuestionAnswerUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data MultiChoiceQuestion =
  MultiChoiceQuestion
    { _multiChoiceQuestionUuid :: U.UUID
    , _multiChoiceQuestionTitle :: String
    , _multiChoiceQuestionText :: Maybe String
    , _multiChoiceQuestionRequiredPhaseUuid :: Maybe U.UUID
    , _multiChoiceQuestionAnnotations :: [MapEntry String String]
    , _multiChoiceQuestionTagUuids :: [U.UUID]
    , _multiChoiceQuestionExpertUuids :: [U.UUID]
    , _multiChoiceQuestionReferenceUuids :: [U.UUID]
    , _multiChoiceQuestionChoiceUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data ListQuestion =
  ListQuestion
    { _listQuestionUuid :: U.UUID
    , _listQuestionTitle :: String
    , _listQuestionText :: Maybe String
    , _listQuestionRequiredPhaseUuid :: Maybe U.UUID
    , _listQuestionAnnotations :: [MapEntry String String]
    , _listQuestionTagUuids :: [U.UUID]
    , _listQuestionExpertUuids :: [U.UUID]
    , _listQuestionReferenceUuids :: [U.UUID]
    , _listQuestionItemTemplateQuestionUuids :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data ValueQuestion =
  ValueQuestion
    { _valueQuestionUuid :: U.UUID
    , _valueQuestionTitle :: String
    , _valueQuestionText :: Maybe String
    , _valueQuestionRequiredPhaseUuid :: Maybe U.UUID
    , _valueQuestionAnnotations :: [MapEntry String String]
    , _valueQuestionTagUuids :: [U.UUID]
    , _valueQuestionExpertUuids :: [U.UUID]
    , _valueQuestionReferenceUuids :: [U.UUID]
    , _valueQuestionValueType :: QuestionValueType
    }
  deriving (Show, Eq, Generic)

data IntegrationQuestion =
  IntegrationQuestion
    { _integrationQuestionUuid :: U.UUID
    , _integrationQuestionTitle :: String
    , _integrationQuestionText :: Maybe String
    , _integrationQuestionRequiredPhaseUuid :: Maybe U.UUID
    , _integrationQuestionAnnotations :: [MapEntry String String]
    , _integrationQuestionTagUuids :: [U.UUID]
    , _integrationQuestionExpertUuids :: [U.UUID]
    , _integrationQuestionReferenceUuids :: [U.UUID]
    , _integrationQuestionIntegrationUuid :: U.UUID
    , _integrationQuestionProps :: Map String String
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Answer =
  Answer
    { _answerUuid :: U.UUID
    , _answerLabel :: String
    , _answerAdvice :: Maybe String
    , _answerAnnotations :: [MapEntry String String]
    , _answerFollowUpUuids :: [U.UUID]
    , _answerMetricMeasures :: [MetricMeasure]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Choice =
  Choice
    { _choiceUuid :: U.UUID
    , _choiceLabel :: String
    , _choiceAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Expert =
  Expert
    { _expertUuid :: U.UUID
    , _expertName :: String
    , _expertEmail :: String
    , _expertAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Reference
  = ResourcePageReference' ResourcePageReference
  | URLReference' URLReference
  | CrossReference' CrossReference
  deriving (Show, Eq, Generic)

data ResourcePageReference =
  ResourcePageReference
    { _resourcePageReferenceUuid :: U.UUID
    , _resourcePageReferenceShortUuid :: String
    , _resourcePageReferenceAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

data URLReference =
  URLReference
    { _uRLReferenceUuid :: U.UUID
    , _uRLReferenceUrl :: String
    , _uRLReferenceLabel :: String
    , _uRLReferenceAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

data CrossReference =
  CrossReference
    { _crossReferenceUuid :: U.UUID
    , _crossReferenceTargetUuid :: U.UUID
    , _crossReferenceDescription :: String
    , _crossReferenceAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Metric =
  Metric
    { _metricUuid :: U.UUID
    , _metricTitle :: String
    , _metricAbbreviation :: Maybe String
    , _metricDescription :: Maybe String
    , _metricAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

data MetricMeasure =
  MetricMeasure
    { _metricMeasureMetricUuid :: U.UUID
    , _metricMeasureMeasure :: Double
    , _metricMeasureWeight :: Double
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Phase =
  Phase
    { _phaseUuid :: U.UUID
    , _phaseTitle :: String
    , _phaseDescription :: Maybe String
    , _phaseAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Tag =
  Tag
    { _tagUuid :: U.UUID
    , _tagName :: String
    , _tagDescription :: Maybe String
    , _tagColor :: String
    , _tagAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Integration
  = ApiIntegration' ApiIntegration
  | WidgetIntegration' WidgetIntegration
  deriving (Show, Eq, Generic)

data ApiIntegration =
  ApiIntegration
    { _apiIntegrationUuid :: U.UUID
    , _apiIntegrationIId :: String
    , _apiIntegrationName :: String
    , _apiIntegrationProps :: [String]
    , _apiIntegrationLogo :: String
    , _apiIntegrationRequestMethod :: String
    , _apiIntegrationRequestUrl :: String
    , _apiIntegrationRequestHeaders :: [MapEntry String String]
    , _apiIntegrationRequestBody :: String
    , _apiIntegrationRequestEmptySearch :: Bool
    , _apiIntegrationResponseListField :: String
    , _apiIntegrationResponseItemId :: String
    , _apiIntegrationResponseItemTemplate :: String
    , _apiIntegrationItemUrl :: String
    , _apiIntegrationAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)

data WidgetIntegration =
  WidgetIntegration
    { _widgetIntegrationUuid :: U.UUID
    , _widgetIntegrationIId :: String
    , _widgetIntegrationName :: String
    , _widgetIntegrationProps :: [String]
    , _widgetIntegrationLogo :: String
    , _widgetIntegrationWidgetUrl :: String
    , _widgetIntegrationItemUrl :: String
    , _widgetIntegrationAnnotations :: [MapEntry String String]
    }
  deriving (Show, Eq, Generic)
