module Api.Resources.KnowledgeModel.KnowledgeModelDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data KnowledgeModelDTO = KnowledgeModelDTO
  { _kmdtoUuid :: UUID
  , _kmdtoName :: String
  , _kmdtoChapters :: [ChapterDTO]
  } deriving (Show, Eq)

data ChapterDTO = ChapterDTO
  { _chdtoUuid :: UUID
  , _chdtoNamespace :: String
  , _chdtoFormatVersion :: Int
  , _chdtoTitle :: String
  , _chdtoText :: String
  , _chdtoQuestions :: [QuestionDTO]
  } deriving (Show, Eq)

data QuestionDTO = QuestionDTO
  { _qdtoUuid :: UUID
  , _qdtoShortUuid :: Maybe String
  , _qdtoType :: String
  , _qdtoTitle :: String
  , _qdtoText :: String
  , _qdtoAnswers :: [AnswerDTO]
  , _qdtoReferences :: [ReferenceDTO]
  , _qdtoExperts :: [ExpertDTO]
  } deriving (Show, Eq)

data AnswerDTO = AnswerDTO
  { _ansdtoUuid :: UUID
  , _ansdtoLabel :: String
  , _ansdtoAdvice :: Maybe String
  , _ansdtoFollowing :: [QuestionDTO]
  } deriving (Show, Eq)

data ExpertDTO = ExpertDTO
  { _expdtoUuid :: UUID
  , _expdtoName :: String
  , _expdtoEmail :: String
  } deriving (Show, Eq)

data ReferenceDTO = ReferenceDTO
  { _refdtoUuid :: UUID
  , _refdtoChapter :: String
  } deriving (Show, Eq)

makeLenses ''KnowledgeModelDTO

makeLenses ''ChapterDTO

makeLenses ''QuestionDTO

makeLenses ''AnswerDTO

makeLenses ''ExpertDTO

makeLenses ''ReferenceDTO

instance ToJSON KnowledgeModelDTO where
  toJSON KnowledgeModelDTO {..} =
    object
      ["uuid" .= _kmdtoUuid, "name" .= _kmdtoName, "chapters" .= _kmdtoChapters]

instance ToJSON ChapterDTO where
  toJSON ChapterDTO {..} =
    object
      [ "uuid" .= _chdtoUuid
      , "namespace" .= _chdtoNamespace
      , "formatVersion" .= _chdtoFormatVersion
      , "title" .= _chdtoTitle
      , "text" .= _chdtoText
      , "questions" .= _chdtoQuestions
      ]

instance ToJSON QuestionDTO where
  toJSON QuestionDTO {..} =
    object
      [ "uuid" .= _qdtoUuid
      , "shortUuid" .= _qdtoShortUuid
      , "type" .= _qdtoType
      , "title" .= _qdtoTitle
      , "text" .= _qdtoText
      , "answers" .= _qdtoAnswers
      , "references" .= _qdtoReferences
      , "experts" .= _qdtoExperts
      ]

instance ToJSON AnswerDTO where
  toJSON AnswerDTO {..} =
    object
      [ "uuid" .= _ansdtoUuid
      , "label" .= _ansdtoLabel
      , "advice" .= _ansdtoAdvice
      , "following" .= _ansdtoFollowing
      ]

instance ToJSON ExpertDTO where
  toJSON ExpertDTO {..} =
    object
      ["uuid" .= _expdtoUuid, "name" .= _expdtoName, "email" .= _expdtoEmail]

instance ToJSON ReferenceDTO where
  toJSON ReferenceDTO {..} =
    object ["uuid" .= _refdtoUuid, "chapter" .= _refdtoChapter]
