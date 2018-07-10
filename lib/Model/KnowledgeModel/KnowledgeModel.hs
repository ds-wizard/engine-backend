module Model.KnowledgeModel.KnowledgeModel where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data QuestionType
  = QuestionTypeOptions
  | QuestionTypeList
  | QuestionTypeString
  | QuestionTypeNumber
  | QuestionTypeDate
  | QuestionTypeText
  deriving (Show, Eq, Generic)

data KnowledgeModel = KnowledgeModel
  { _knowledgeModelUuid :: U.UUID
  , _knowledgeModelName :: String
  , _knowledgeModelChapters :: [Chapter]
  } deriving (Show, Eq, Generic)

data Chapter = Chapter
  { _chapterUuid :: U.UUID
  , _chapterTitle :: String
  , _chapterText :: String
  , _chapterQuestions :: [Question]
  } deriving (Show, Eq, Generic)

data Question = Question
  { _questionUuid :: U.UUID
  , _questionQType :: QuestionType
  , _questionTitle :: String
  , _questionText :: String
  , _questionAnswerItemTemplate :: Maybe AnswerItemTemplate
  , _questionAnswers :: Maybe [Answer]
  , _questionExperts :: [Expert]
  , _questionReferences :: [Reference]
  } deriving (Show, Eq, Generic)

data Answer = Answer
  { _answerUuid :: U.UUID
  , _answerLabel :: String
  , _answerAdvice :: Maybe String
  , _answerFollowUps :: [Question]
  , _answerMetricMeasures :: [MetricMeasure]
  } deriving (Show, Eq, Generic)

data AnswerItemTemplate = AnswerItemTemplate
  { _answerItemTemplateTitle :: String
  , _answerItemTemplateQuestions :: [Question]
  } deriving (Show, Eq, Generic)

data AnswerItemTemplatePlain = AnswerItemTemplatePlain
  { _answerItemTemplatePlainTitle :: String
  } deriving (Show, Eq, Generic)

data AnswerItemTemplatePlainWithIds = AnswerItemTemplatePlainWithIds
  { _answerItemTemplatePlainWithIdsTitle :: String
  , _answerItemTemplatePlainWithIdsQuestionIds :: [U.UUID]
  } deriving (Show, Eq, Generic)

data Expert = Expert
  { _expertUuid :: U.UUID
  , _expertName :: String
  , _expertEmail :: String
  } deriving (Show, Eq, Generic)

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
  , _uRLReferenceAnchor :: String
  } deriving (Show, Eq, Generic)

data CrossReference = CrossReference
  { _crossReferenceUuid :: U.UUID
  , _crossReferenceTargetUuid :: U.UUID
  , _crossReferenceDescription :: String
  } deriving (Show, Eq, Generic)

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
