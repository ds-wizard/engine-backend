module Model.KnowledgeModel.KnowledgeModel where

import Data.UUID
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
  , _questionAnswerItemTemplate :: Maybe AnswerItemTemplate
  , _questionAnswers :: Maybe [Answer]
  , _questionExperts :: [Expert]
  , _questionReferences :: [Reference]
  } deriving (Show, Eq, Generic)

data Answer = Answer
  { _answerUuid :: UUID
  , _answerLabel :: String
  , _answerAdvice :: Maybe String
  , _answerFollowUps :: [Question]
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
  , _answerItemTemplatePlainWithIdsQuestionIds :: [UUID]
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
