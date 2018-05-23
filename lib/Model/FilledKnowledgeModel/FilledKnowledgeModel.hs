module Model.FilledKnowledgeModel.FilledKnowledgeModel where

import Data.UUID
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

data FilledKnowledgeModel = FilledKnowledgeModel
  { _filledKnowledgeModelUuid :: UUID
  , _filledKnowledgeModelName :: String
  , _filledKnowledgeModelChapters :: [FilledChapter]
  } deriving (Show, Eq, Generic)

data FilledChapter = FilledChapter
  { _filledChapterUuid :: UUID
  , _filledChapterTitle :: String
  , _filledChapterText :: String
  , _filledChapterQuestions :: [FilledQuestion]
  } deriving (Show, Eq, Generic)

data FilledQuestion = FilledQuestion
  { _filledQuestionUuid :: UUID
  , _filledQuestionShortUuid :: Maybe String
  , _filledQuestionQType :: QuestionType
  , _filledQuestionTitle :: String
  , _filledQuestionText :: String
  , _filledQuestionAnswerValue :: Maybe String
  , _filledQuestionAnswerOption :: Maybe FilledAnswer
  , _filledQuestionAnswerItems :: Maybe [FilledAnswerItem]
  , _filledQuestionExperts :: [Expert]
  , _filledQuestionReferences :: [Reference]
  } deriving (Show, Eq, Generic)

data FilledAnswer = FilledAnswer
  { _filledAnswerUuid :: UUID
  , _filledAnswerLabel :: String
  , _filledAnswerAdvice :: Maybe String
  , _filledAnswerFollowUps :: [FilledQuestion]
  } deriving (Show, Eq, Generic)

data FilledAnswerItem = FilledAnswerItem
  { _filledAnswerItemTitle :: String
  , _filledAnswerItemValue :: String
  , _filledAnswerItemQuestions :: [FilledQuestion]
  } deriving (Show, Eq, Generic)
