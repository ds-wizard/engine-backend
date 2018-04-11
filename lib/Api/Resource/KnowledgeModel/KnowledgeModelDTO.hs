module Api.Resource.KnowledgeModel.KnowledgeModelDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Api.Resource.Common
import Common.Types
import Common.Uuid
import Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelDTO = KnowledgeModelDTO
  { _knowledgeModelDTOUuid :: UUID
  , _knowledgeModelDTOName :: String
  , _knowledgeModelDTOChapters :: [ChapterDTO]
  } deriving (Show, Eq)

data ChapterDTO = ChapterDTO
  { _chapterDTOUuid :: UUID
  , _chapterDTOTitle :: String
  , _chapterDTOText :: String
  , _chapterDTOQuestions :: [QuestionDTO]
  } deriving (Show, Eq)

data QuestionDTO = QuestionDTO
  { _questionDTOUuid :: UUID
  , _questionDTOShortUuid :: Maybe String
  , _questionDTOQType :: QuestionType
  , _questionDTOTitle :: String
  , _questionDTOText :: String
  , _questionDTOAnswers :: [AnswerDTO]
  , _questionDTOReferences :: [ReferenceDTO]
  , _questionDTOExperts :: [ExpertDTO]
  } deriving (Show, Eq)

data AnswerDTO = AnswerDTO
  { _answerDTOUuid :: UUID
  , _answerDTOLabel :: String
  , _answerDTOAdvice :: Maybe String
  , _answerDTOFollowUps :: [QuestionDTO]
  } deriving (Show, Eq)

data ExpertDTO = ExpertDTO
  { _expertDTOUuid :: UUID
  , _expertDTOName :: String
  , _expertDTOEmail :: String
  } deriving (Show, Eq)

data ReferenceDTO = ReferenceDTO
  { _referenceDTOUuid :: UUID
  , _referenceDTOChapter :: String
  } deriving (Show, Eq)

instance ToJSON KnowledgeModelDTO where
  toJSON KnowledgeModelDTO {..} =
    object
      ["uuid" .= _knowledgeModelDTOUuid, "name" .= _knowledgeModelDTOName, "chapters" .= _knowledgeModelDTOChapters]

instance ToJSON ChapterDTO where
  toJSON ChapterDTO {..} =
    object
      [ "uuid" .= _chapterDTOUuid
      , "title" .= _chapterDTOTitle
      , "text" .= _chapterDTOText
      , "questions" .= _chapterDTOQuestions
      ]

instance ToJSON QuestionDTO where
  toJSON QuestionDTO {..} =
    object
      [ "uuid" .= _questionDTOUuid
      , "shortUuid" .= _questionDTOShortUuid
      , "type" .= serializeQuestionType _questionDTOQType
      , "title" .= _questionDTOTitle
      , "text" .= _questionDTOText
      , "answers" .= _questionDTOAnswers
      , "references" .= _questionDTOReferences
      , "experts" .= _questionDTOExperts
      ]

instance ToJSON AnswerDTO where
  toJSON AnswerDTO {..} =
    object
      [ "uuid" .= _answerDTOUuid
      , "label" .= _answerDTOLabel
      , "advice" .= _answerDTOAdvice
      , "followUps" .= _answerDTOFollowUps
      ]

instance ToJSON ExpertDTO where
  toJSON ExpertDTO {..} = object ["uuid" .= _expertDTOUuid, "name" .= _expertDTOName, "email" .= _expertDTOEmail]

instance ToJSON ReferenceDTO where
  toJSON ReferenceDTO {..} = object ["uuid" .= _referenceDTOUuid, "chapter" .= _referenceDTOChapter]

instance FromJSON KnowledgeModelDTO where
  parseJSON (Object o) = do
    _knowledgeModelDTOUuid <- o .: "uuid"
    _knowledgeModelDTOName <- o .: "name"
    _knowledgeModelDTOChapters <- o .: "chapters"
    return KnowledgeModelDTO {..}
  parseJSON _ = mzero

instance FromJSON ChapterDTO where
  parseJSON (Object o) = do
    _chapterDTOUuid <- o .: "uuid"
    _chapterDTOTitle <- o .: "title"
    _chapterDTOText <- o .: "text"
    _chapterDTOQuestions <- o .: "questions"
    return ChapterDTO {..}
  parseJSON _ = mzero

instance FromJSON QuestionDTO where
  parseJSON (Object o) = do
    _questionDTOUuid <- o .: "uuid"
    _questionDTOShortUuid <- o .: "shortUuid"
    _questionDTOTitle <- o .: "title"
    _questionDTOText <- o .: "text"
    _questionDTOReferences <- o .: "answers"
    _questionDTOAnswers <- o .: "references"
    _questionDTOExperts <- o .: "experts"
    questionType <- o .: "type"
    case deserializeQuestionType questionType of
      (Just _questionDTOQType) -> return QuestionDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance FromJSON AnswerDTO where
  parseJSON (Object o) = do
    _answerDTOUuid <- o .: "uuid"
    _answerDTOLabel <- o .: "label"
    _answerDTOAdvice <- o .: "advice"
    _answerDTOFollowUps <- o .: "followUps"
    return AnswerDTO {..}
  parseJSON _ = mzero

instance FromJSON ExpertDTO where
  parseJSON (Object o) = do
    _expertDTOUuid <- o .: "uuid"
    _expertDTOName <- o .: "name"
    _expertDTOEmail <- o .: "email"
    return ExpertDTO {..}
  parseJSON _ = mzero

instance FromJSON ReferenceDTO where
  parseJSON (Object o) = do
    _referenceDTOUuid <- o .: "uuid"
    _referenceDTOChapter <- o .: "chapter"
    return ReferenceDTO {..}
  parseJSON _ = mzero
