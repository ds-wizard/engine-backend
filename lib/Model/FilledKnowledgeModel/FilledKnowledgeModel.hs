module Model.FilledKnowledgeModel.FilledKnowledgeModel where

import qualified Data.UUID as U
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

data FilledKnowledgeModel = FilledKnowledgeModel
  { _filledKnowledgeModelUuid :: U.UUID
  , _filledKnowledgeModelName :: String
  , _filledKnowledgeModelChapters :: [FilledChapter]
  , _filledKnowledgeModelTags :: [Tag]
  } deriving (Show, Eq, Generic)

data FilledChapter = FilledChapter
  { _filledChapterUuid :: U.UUID
  , _filledChapterTitle :: String
  , _filledChapterText :: String
  , _filledChapterQuestions :: [FilledQuestion]
  } deriving (Show, Eq, Generic)

data FilledQuestion = FilledQuestion
  { _filledQuestionUuid :: U.UUID
  , _filledQuestionQType :: QuestionType
  , _filledQuestionTitle :: String
  , _filledQuestionText :: Maybe String
  , _filledQuestionRequiredLevel :: Maybe Int
  , _filledQuestionTagUuids :: [U.UUID]
  , _filledQuestionAnswerItemTemplate :: Maybe AnswerItemTemplate
  , _filledQuestionAnswers :: Maybe [Answer]
  , _filledQuestionAnswerValue :: Maybe String
  , _filledQuestionAnswerOption :: Maybe FilledAnswer
  , _filledQuestionAnswerItems :: Maybe [FilledAnswerItem]
  , _filledQuestionExperts :: [Expert]
  , _filledQuestionReferences :: [Reference]
  } deriving (Show, Eq, Generic)

data FilledAnswer = FilledAnswer
  { _filledAnswerUuid :: U.UUID
  , _filledAnswerLabel :: String
  , _filledAnswerAdvice :: Maybe String
  , _filledAnswerFollowUps :: [FilledQuestion]
  , _filledAnswerMetricMeasures :: [MetricMeasure]
  } deriving (Show, Eq, Generic)

data FilledAnswerItem = FilledAnswerItem
  { _filledAnswerItemTitle :: String
  , _filledAnswerItemValue :: Maybe String
  , _filledAnswerItemQuestions :: [FilledQuestion]
  } deriving (Show, Eq, Generic)
