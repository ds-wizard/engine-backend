module Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO where

import Control.Monad
import Data.Aeson
import Data.UUID

import Api.Resource.Common
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.KnowledgeModel.KnowledgeModel

data FilledKnowledgeModelDTO = FilledKnowledgeModelDTO
  { _filledKnowledgeModelDTOUuid :: UUID
  , _filledKnowledgeModelDTOName :: String
  , _filledKnowledgeModelDTOChapters :: [FilledChapterDTO]
  } deriving (Show, Eq)

data FilledChapterDTO = FilledChapterDTO
  { _filledChapterDTOUuid :: UUID
  , _filledChapterDTOTitle :: String
  , _filledChapterDTOText :: String
  , _filledChapterDTOQuestions :: [FilledQuestionDTO]
  } deriving (Show, Eq)

data FilledQuestionDTO = FilledQuestionDTO
  { _filledQuestionDTOUuid :: UUID
  , _filledQuestionDTOQType :: QuestionType
  , _filledQuestionDTOTitle :: String
  , _filledQuestionDTOText :: Maybe String
  , _filledQuestionDTORequiredLevel :: Maybe Int
  , _filledQuestionDTOAnswerItemTemplate :: Maybe AnswerItemTemplateDTO
  , _filledQuestionDTOAnswers :: Maybe [AnswerDTO]
  , _filledQuestionDTOAnswerValue :: Maybe String
  , _filledQuestionDTOAnswerOption :: Maybe FilledAnswerDTO
  , _filledQuestionDTOAnswerItems :: Maybe [FilledAnswerItemDTO]
  , _filledQuestionDTOExperts :: [ExpertDTO]
  , _filledQuestionDTOReferences :: [ReferenceDTO]
  } deriving (Show, Eq)

data FilledAnswerDTO = FilledAnswerDTO
  { _filledAnswerDTOUuid :: UUID
  , _filledAnswerDTOLabel :: String
  , _filledAnswerDTOAdvice :: Maybe String
  , _filledAnswerDTOFollowUps :: [FilledQuestionDTO]
  , _filledAnswerDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq)

data FilledAnswerItemDTO = FilledAnswerItemDTO
  { _filledAnswerItemDTOTitle :: String
  , _filledAnswerItemDTOValue :: String
  , _filledAnswerItemDTOQuestions :: [FilledQuestionDTO]
  } deriving (Show, Eq)

instance ToJSON FilledKnowledgeModelDTO where
  toJSON FilledKnowledgeModelDTO {..} =
    object
      [ "uuid" .= _filledKnowledgeModelDTOUuid
      , "name" .= _filledKnowledgeModelDTOName
      , "chapters" .= _filledKnowledgeModelDTOChapters
      ]

instance ToJSON FilledChapterDTO where
  toJSON FilledChapterDTO {..} =
    object
      [ "uuid" .= _filledChapterDTOUuid
      , "title" .= _filledChapterDTOTitle
      , "text" .= _filledChapterDTOText
      , "questions" .= _filledChapterDTOQuestions
      ]

instance ToJSON FilledQuestionDTO where
  toJSON FilledQuestionDTO {..} =
    object
      [ "uuid" .= _filledQuestionDTOUuid
      , "type" .= serializeQuestionType _filledQuestionDTOQType
      , "title" .= _filledQuestionDTOTitle
      , "text" .= _filledQuestionDTOText
      , "requiredLevel" .= _filledQuestionDTORequiredLevel
      , "answerItemTemplate" .= _filledQuestionDTOAnswerItemTemplate
      , "answers" .= _filledQuestionDTOAnswers
      , "answerValue" .= _filledQuestionDTOAnswerValue
      , "answerOption" .= _filledQuestionDTOAnswerOption
      , "answerItems" .= _filledQuestionDTOAnswerItems
      , "references" .= _filledQuestionDTOReferences
      , "experts" .= _filledQuestionDTOExperts
      ]

instance ToJSON FilledAnswerDTO where
  toJSON FilledAnswerDTO {..} =
    object
      [ "uuid" .= _filledAnswerDTOUuid
      , "label" .= _filledAnswerDTOLabel
      , "advice" .= _filledAnswerDTOAdvice
      , "followUps" .= _filledAnswerDTOFollowUps
      , "metricMeasures" .= _filledAnswerDTOMetricMeasures
      ]

instance ToJSON FilledAnswerItemDTO where
  toJSON FilledAnswerItemDTO {..} =
    object
      [ "title" .= _filledAnswerItemDTOTitle
      , "value" .= _filledAnswerItemDTOValue
      , "questions" .= _filledAnswerItemDTOQuestions
      ]

instance FromJSON FilledKnowledgeModelDTO where
  parseJSON (Object o) = do
    _filledKnowledgeModelDTOUuid <- o .: "uuid"
    _filledKnowledgeModelDTOName <- o .: "name"
    _filledKnowledgeModelDTOChapters <- o .: "chapters"
    return FilledKnowledgeModelDTO {..}
  parseJSON _ = mzero

instance FromJSON FilledChapterDTO where
  parseJSON (Object o) = do
    _filledChapterDTOUuid <- o .: "uuid"
    _filledChapterDTOTitle <- o .: "title"
    _filledChapterDTOText <- o .: "text"
    _filledChapterDTOQuestions <- o .: "questions"
    return FilledChapterDTO {..}
  parseJSON _ = mzero

instance FromJSON FilledQuestionDTO where
  parseJSON (Object o) = do
    _filledQuestionDTOUuid <- o .: "uuid"
    _filledQuestionDTOTitle <- o .: "title"
    _filledQuestionDTOText <- o .: "text"
    _filledQuestionDTORequiredLevel <- o .: "requiredLevel"
    _filledQuestionDTOAnswerItemTemplate <- o .: "answerItemTemplate"
    _filledQuestionDTOAnswers <- o .: "answers"
    _filledQuestionDTOAnswerValue <- o .: "answerValue"
    _filledQuestionDTOAnswerOption <- o .: "answerOption"
    _filledQuestionDTOAnswerItems <- o .: "answerItems"
    _filledQuestionDTOExperts <- o .: "experts"
    _filledQuestionDTOReferences <- o .: "answers"
    questionType <- o .: "type"
    case deserializeQuestionType questionType of
      (Just _filledQuestionDTOQType) -> return FilledQuestionDTO {..}
      Nothing -> fail "Unsupported question type"
  parseJSON _ = mzero

instance FromJSON FilledAnswerDTO where
  parseJSON (Object o) = do
    _filledAnswerDTOUuid <- o .: "uuid"
    _filledAnswerDTOLabel <- o .: "label"
    _filledAnswerDTOAdvice <- o .: "advice"
    _filledAnswerDTOFollowUps <- o .: "followUps"
    _filledAnswerDTOMetricMeasures <- o .: "metricMeasures"
    return FilledAnswerDTO {..}
  parseJSON _ = mzero

instance FromJSON FilledAnswerItemDTO where
  parseJSON (Object o) = do
    _filledAnswerItemDTOTitle <- o .: "title"
    _filledAnswerItemDTOValue <- o .: "value"
    _filledAnswerItemDTOQuestions <- o .: "questions"
    return FilledAnswerItemDTO {..}
  parseJSON _ = mzero
