module Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO where

import Control.Monad
import Data.Aeson
import Data.Map
import qualified Data.UUID as U

import Api.Resource.Common
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.KnowledgeModel.KnowledgeModel

data FilledKnowledgeModelDTO = FilledKnowledgeModelDTO
  { _filledKnowledgeModelDTOUuid :: U.UUID
  , _filledKnowledgeModelDTOName :: String
  , _filledKnowledgeModelDTOChapters :: [FilledChapterDTO]
  , _filledKnowledgeModelDTOTags :: [TagDTO]
  , _filledKnowledgeModelDTOIntegrations :: [IntegrationDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data FilledChapterDTO = FilledChapterDTO
  { _filledChapterDTOUuid :: U.UUID
  , _filledChapterDTOHumanIdentifier :: String
  , _filledChapterDTOTitle :: String
  , _filledChapterDTOText :: String
  , _filledChapterDTOQuestions :: [FilledQuestionDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data FilledQuestionDTO
  = FilledOptionsQuestionDTO' FilledOptionsQuestionDTO
  | FilledListQuestionDTO' FilledListQuestionDTO
  | FilledValueQuestionDTO' FilledValueQuestionDTO
  | FilledIntegrationQuestionDTO' FilledIntegrationQuestionDTO
  deriving (Show, Eq)

data FilledOptionsQuestionDTO = FilledOptionsQuestionDTO
  { _filledOptionsQuestionDTOUuid :: U.UUID
  , _filledOptionsQuestionDTOHumanIdentifier :: String
  , _filledOptionsQuestionDTOTitle :: String
  , _filledOptionsQuestionDTOText :: Maybe String
  , _filledOptionsQuestionDTORequiredLevel :: Maybe Int
  , _filledOptionsQuestionDTOTagUuids :: [U.UUID]
  , _filledOptionsQuestionDTOExperts :: [ExpertDTO]
  , _filledOptionsQuestionDTOReferences :: [ReferenceDTO]
  , _filledOptionsQuestionDTOAnswers :: [AnswerDTO]
  , _filledOptionsQuestionDTOAnswerOption :: Maybe FilledAnswerDTO
  } deriving (Show, Eq)

data FilledListQuestionDTO = FilledListQuestionDTO
  { _filledListQuestionDTOUuid :: U.UUID
  , _filledListQuestionDTOHumanIdentifier :: String
  , _filledListQuestionDTOTitle :: String
  , _filledListQuestionDTOText :: Maybe String
  , _filledListQuestionDTORequiredLevel :: Maybe Int
  , _filledListQuestionDTOTagUuids :: [U.UUID]
  , _filledListQuestionDTOExperts :: [ExpertDTO]
  , _filledListQuestionDTOReferences :: [ReferenceDTO]
  , _filledListQuestionDTOItemTemplateTitle :: String
  , _filledListQuestionDTOItemTemplateQuestions :: [QuestionDTO]
  , _filledListQuestionDTOItems :: Maybe [FilledAnswerItemDTO]
  } deriving (Show, Eq)

data FilledValueQuestionDTO = FilledValueQuestionDTO
  { _filledValueQuestionDTOUuid :: U.UUID
  , _filledValueQuestionDTOHumanIdentifier :: String
  , _filledValueQuestionDTOTitle :: String
  , _filledValueQuestionDTOText :: Maybe String
  , _filledValueQuestionDTORequiredLevel :: Maybe Int
  , _filledValueQuestionDTOTagUuids :: [U.UUID]
  , _filledValueQuestionDTOExperts :: [ExpertDTO]
  , _filledValueQuestionDTOReferences :: [ReferenceDTO]
  , _filledValueQuestionDTOValueType :: QuestionValueType
  , _filledValueQuestionDTOAnswerValue :: Maybe String
  } deriving (Show, Eq)

data FilledIntegrationQuestionDTO = FilledIntegrationQuestionDTO
  { _filledIntegrationQuestionDTOUuid :: U.UUID
  , _filledIntegrationQuestionDTOHumanIdentifier :: String
  , _filledIntegrationQuestionDTOTitle :: String
  , _filledIntegrationQuestionDTOText :: Maybe String
  , _filledIntegrationQuestionDTORequiredLevel :: Maybe Int
  , _filledIntegrationQuestionDTOTagUuids :: [U.UUID]
  , _filledIntegrationQuestionDTOExperts :: [ExpertDTO]
  , _filledIntegrationQuestionDTOReferences :: [ReferenceDTO]
  , _filledIntegrationQuestionDTOIntegrationUuid :: U.UUID
  , _filledIntegrationQuestionDTOProps :: Map String String
  , _filledIntegrationQuestionDTOAnswerIntId :: Maybe String
  , _filledIntegrationQuestionDTOAnswerValue :: Maybe String
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
data FilledAnswerDTO = FilledAnswerDTO
  { _filledAnswerDTOUuid :: U.UUID
  , _filledAnswerDTOHumanIdentifier :: String
  , _filledAnswerDTOLabel :: String
  , _filledAnswerDTOAdvice :: Maybe String
  , _filledAnswerDTOFollowUps :: [FilledQuestionDTO]
  , _filledAnswerDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq)

data FilledAnswerItemDTO = FilledAnswerItemDTO
  { _filledAnswerItemDTOTitle :: String
  , _filledAnswerItemDTOHumanIdentifier :: String
  , _filledAnswerItemDTOValue :: Maybe String
  , _filledAnswerItemDTOQuestions :: [FilledQuestionDTO]
  } deriving (Show, Eq)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON FilledKnowledgeModelDTO where
  toJSON FilledKnowledgeModelDTO {..} =
    object
      [ "uuid" .= _filledKnowledgeModelDTOUuid
      , "name" .= _filledKnowledgeModelDTOName
      , "chapters" .= _filledKnowledgeModelDTOChapters
      , "tags" .= _filledKnowledgeModelDTOTags
      , "integrations" .= _filledKnowledgeModelDTOIntegrations
      ]

-- --------------------------------------------------------------------
instance ToJSON FilledChapterDTO where
  toJSON FilledChapterDTO {..} =
    object
      [ "uuid" .= _filledChapterDTOUuid
      , "humanIdentifier" .= _filledChapterDTOHumanIdentifier
      , "title" .= _filledChapterDTOTitle
      , "text" .= _filledChapterDTOText
      , "questions" .= _filledChapterDTOQuestions
      ]

-- --------------------------------------------------------------------
instance ToJSON FilledQuestionDTO where
  toJSON (FilledOptionsQuestionDTO' event) = toJSON event
  toJSON (FilledListQuestionDTO' event) = toJSON event
  toJSON (FilledValueQuestionDTO' event) = toJSON event
  toJSON (FilledIntegrationQuestionDTO' event) = toJSON event

instance ToJSON FilledOptionsQuestionDTO where
  toJSON FilledOptionsQuestionDTO {..} =
    object
      [ "questionType" .= "OptionsQuestion"
      , "uuid" .= _filledOptionsQuestionDTOUuid
      , "humanIdentifier" .= _filledOptionsQuestionDTOHumanIdentifier
      , "title" .= _filledOptionsQuestionDTOTitle
      , "text" .= _filledOptionsQuestionDTOText
      , "requiredLevel" .= _filledOptionsQuestionDTORequiredLevel
      , "tagUuids" .= _filledOptionsQuestionDTOTagUuids
      , "references" .= _filledOptionsQuestionDTOReferences
      , "experts" .= _filledOptionsQuestionDTOExperts
      , "answers" .= _filledOptionsQuestionDTOAnswers
      , "answerOption" .= _filledOptionsQuestionDTOAnswerOption
      ]

instance ToJSON FilledListQuestionDTO where
  toJSON FilledListQuestionDTO {..} =
    object
      [ "questionType" .= "ListQuestion"
      , "uuid" .= _filledListQuestionDTOUuid
      , "humanIdentifier" .= _filledListQuestionDTOHumanIdentifier
      , "title" .= _filledListQuestionDTOTitle
      , "text" .= _filledListQuestionDTOText
      , "requiredLevel" .= _filledListQuestionDTORequiredLevel
      , "tagUuids" .= _filledListQuestionDTOTagUuids
      , "experts" .= _filledListQuestionDTOExperts
      , "references" .= _filledListQuestionDTOReferences
      , "itemTemplateTitle" .= _filledListQuestionDTOItemTemplateTitle
      , "itemTemplateQuestions" .= _filledListQuestionDTOItemTemplateQuestions
      , "items" .= _filledListQuestionDTOItems
      ]

instance ToJSON FilledValueQuestionDTO where
  toJSON FilledValueQuestionDTO {..} =
    object
      [ "questionType" .= "ValueQuestion"
      , "uuid" .= _filledValueQuestionDTOUuid
      , "humanIdentifier" .= _filledValueQuestionDTOHumanIdentifier
      , "title" .= _filledValueQuestionDTOTitle
      , "text" .= _filledValueQuestionDTOText
      , "requiredLevel" .= _filledValueQuestionDTORequiredLevel
      , "tagUuids" .= _filledValueQuestionDTOTagUuids
      , "references" .= _filledValueQuestionDTOReferences
      , "experts" .= _filledValueQuestionDTOExperts
      , "valueType" .= serializeQuestionValueType _filledValueQuestionDTOValueType
      , "answerValue" .= _filledValueQuestionDTOAnswerValue
      ]

instance ToJSON FilledIntegrationQuestionDTO where
  toJSON FilledIntegrationQuestionDTO {..} =
    object
      [ "questionType" .= "IntegrationQuestion"
      , "uuid" .= _filledIntegrationQuestionDTOUuid
      , "humanIdentifier" .= _filledIntegrationQuestionDTOHumanIdentifier
      , "title" .= _filledIntegrationQuestionDTOTitle
      , "text" .= _filledIntegrationQuestionDTOText
      , "requiredLevel" .= _filledIntegrationQuestionDTORequiredLevel
      , "tagUuids" .= _filledIntegrationQuestionDTOTagUuids
      , "references" .= _filledIntegrationQuestionDTOReferences
      , "experts" .= _filledIntegrationQuestionDTOExperts
      , "integrationUuid" .= _filledIntegrationQuestionDTOIntegrationUuid
      , "props" .= _filledIntegrationQuestionDTOProps
      , "answerIntId" .= _filledIntegrationQuestionDTOAnswerIntId
      , "answerValue" .= _filledIntegrationQuestionDTOAnswerValue
      ]

-- --------------------------------------------------------------------
instance ToJSON FilledAnswerDTO where
  toJSON FilledAnswerDTO {..} =
    object
      [ "uuid" .= _filledAnswerDTOUuid
      , "humanIdentifier" .= _filledAnswerDTOHumanIdentifier
      , "label" .= _filledAnswerDTOLabel
      , "advice" .= _filledAnswerDTOAdvice
      , "followUps" .= _filledAnswerDTOFollowUps
      , "metricMeasures" .= _filledAnswerDTOMetricMeasures
      ]

instance ToJSON FilledAnswerItemDTO where
  toJSON FilledAnswerItemDTO {..} =
    object
      [ "humanIdentifier" .= _filledAnswerItemDTOHumanIdentifier
      , "title" .= _filledAnswerItemDTOTitle
      , "value" .= _filledAnswerItemDTOValue
      , "questions" .= _filledAnswerItemDTOQuestions
      ]

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance FromJSON FilledKnowledgeModelDTO where
  parseJSON (Object o) = do
    _filledKnowledgeModelDTOUuid <- o .: "uuid"
    _filledKnowledgeModelDTOName <- o .: "name"
    _filledKnowledgeModelDTOChapters <- o .: "chapters"
    _filledKnowledgeModelDTOTags <- o .: "tags"
    _filledKnowledgeModelDTOIntegrations <- o .: "integrations"
    return FilledKnowledgeModelDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON FilledChapterDTO where
  parseJSON (Object o) = do
    _filledChapterDTOUuid <- o .: "uuid"
    _filledChapterDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledChapterDTOTitle <- o .: "title"
    _filledChapterDTOText <- o .: "text"
    _filledChapterDTOQuestions <- o .: "questions"
    return FilledChapterDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON FilledQuestionDTO where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (FilledOptionsQuestionDTO' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (FilledListQuestionDTO' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (FilledValueQuestionDTO' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (FilledIntegrationQuestionDTO' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

instance FromJSON FilledOptionsQuestionDTO where
  parseJSON (Object o) = do
    _filledOptionsQuestionDTOUuid <- o .: "uuid"
    _filledOptionsQuestionDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledOptionsQuestionDTOTitle <- o .: "title"
    _filledOptionsQuestionDTOText <- o .: "text"
    _filledOptionsQuestionDTORequiredLevel <- o .: "requiredLevel"
    _filledOptionsQuestionDTOTagUuids <- o .: "tagUuids"
    _filledOptionsQuestionDTOExperts <- o .: "experts"
    _filledOptionsQuestionDTOReferences <- o .: "references"
    _filledOptionsQuestionDTOAnswers <- o .: "answers"
    _filledOptionsQuestionDTOAnswerOption <- o .: "answerOption"
    return FilledOptionsQuestionDTO {..}
  parseJSON _ = mzero

instance FromJSON FilledListQuestionDTO where
  parseJSON (Object o) = do
    _filledListQuestionDTOUuid <- o .: "uuid"
    _filledListQuestionDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledListQuestionDTOTitle <- o .: "title"
    _filledListQuestionDTOText <- o .: "text"
    _filledListQuestionDTORequiredLevel <- o .: "requiredLevel"
    _filledListQuestionDTOTagUuids <- o .: "tagUuids"
    _filledListQuestionDTOExperts <- o .: "experts"
    _filledListQuestionDTOReferences <- o .: "references"
    _filledListQuestionDTOItemTemplateTitle <- o .: "itemTemplateTitle"
    _filledListQuestionDTOItemTemplateQuestions <- o .: "itemTemplateQuestions"
    _filledListQuestionDTOItems <- o .: "items"
    return FilledListQuestionDTO {..}
  parseJSON _ = mzero

instance FromJSON FilledValueQuestionDTO where
  parseJSON (Object o) = do
    _filledValueQuestionDTOUuid <- o .: "uuid"
    _filledValueQuestionDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledValueQuestionDTOTitle <- o .: "title"
    _filledValueQuestionDTOText <- o .: "text"
    _filledValueQuestionDTORequiredLevel <- o .: "requiredLevel"
    _filledValueQuestionDTOTagUuids <- o .: "tagUuids"
    _filledValueQuestionDTOExperts <- o .: "experts"
    _filledValueQuestionDTOReferences <- o .: "references"
    _filledValueQuestionDTOAnswerValue <- o .: "answerValue"
    valueType <- o .: "valueType"
    case deserializeQuestionValueType valueType of
      (Just _filledValueQuestionDTOValueType) -> return FilledValueQuestionDTO {..}
      Nothing -> fail "Unsupported question value type"
  parseJSON _ = mzero

instance FromJSON FilledIntegrationQuestionDTO where
  parseJSON (Object o) = do
    _filledIntegrationQuestionDTOUuid <- o .: "uuid"
    _filledIntegrationQuestionDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledIntegrationQuestionDTOTitle <- o .: "title"
    _filledIntegrationQuestionDTOText <- o .: "text"
    _filledIntegrationQuestionDTORequiredLevel <- o .: "requiredLevel"
    _filledIntegrationQuestionDTOTagUuids <- o .: "tagUuids"
    _filledIntegrationQuestionDTOExperts <- o .: "experts"
    _filledIntegrationQuestionDTOReferences <- o .: "references"
    _filledIntegrationQuestionDTOIntegrationUuid <- o .: "integrationUuid"
    _filledIntegrationQuestionDTOProps <- o .: "props"
    _filledIntegrationQuestionDTOAnswerIntId <- o .: "answerIntId"
    _filledIntegrationQuestionDTOAnswerValue <- o .: "answerValue"
    return FilledIntegrationQuestionDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON FilledAnswerDTO where
  parseJSON (Object o) = do
    _filledAnswerDTOUuid <- o .: "uuid"
    _filledAnswerDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledAnswerDTOLabel <- o .: "label"
    _filledAnswerDTOAdvice <- o .: "advice"
    _filledAnswerDTOFollowUps <- o .: "followUps"
    _filledAnswerDTOMetricMeasures <- o .: "metricMeasures"
    return FilledAnswerDTO {..}
  parseJSON _ = mzero

instance FromJSON FilledAnswerItemDTO where
  parseJSON (Object o) = do
    _filledAnswerItemDTOHumanIdentifier <- o .: "humanIdentifier"
    _filledAnswerItemDTOTitle <- o .: "title"
    _filledAnswerItemDTOValue <- o .: "value"
    _filledAnswerItemDTOQuestions <- o .: "questions"
    return FilledAnswerItemDTO {..}
  parseJSON _ = mzero
