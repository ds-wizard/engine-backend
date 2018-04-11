module Model.KnowledgeModel.KnowledgeModel where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

import Model.Common

data QuestionType
  = QuestionTypeOption
  | QuestionTypeList
  | QuestionString
  | QuestionNumber
  | QuestionDate
  | QuestionText
  deriving (Show, Eq, Generic)

data KnowledgeModel = KnowledgeModel
  { _knowledgeModelUuid :: UUID
  , _knowledgeModelName :: String
  , _knowledgeModelChapters :: [Chapter]
  } deriving (Show, Eq, Generic)

data Chapter = Chapter
  { _chapterUuid :: UUID
  , _chapterTitle :: String
  , _chapterText :: String
  , _chapterQuestions :: [Question]
  } deriving (Show, Eq, Generic)

data Question = Question
  { _questionUuid :: UUID
  , _questionShortUuid :: Maybe String
  , _questionQType :: QuestionType
  , _questionTitle :: String
  , _questionText :: String
  , _questionAnswers :: [Answer]
  , _questionExperts :: [Expert]
  , _questionReferences :: [Reference]
  } deriving (Show, Eq, Generic)

data Answer = Answer
  { _answerUuid :: UUID
  , _answerLabel :: String
  , _answerAdvice :: Maybe String
  , _answerFollowUps :: [Question]
  } deriving (Show, Eq, Generic)

data Expert = Expert
  { _expertUuid :: UUID
  , _expertName :: String
  , _expertEmail :: String
  } deriving (Show, Eq, Generic)

data Reference = Reference
  { _referenceUuid :: UUID
  , _referenceChapter :: String
  } deriving (Show, Eq, Generic)
