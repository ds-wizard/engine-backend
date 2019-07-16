module Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO where

import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Model.KnowledgeModel.KnowledgeModel

data FilledKnowledgeModelDTO = FilledKnowledgeModelDTO
  { _filledKnowledgeModelDTOUuid :: U.UUID
  , _filledKnowledgeModelDTOName :: String
  , _filledKnowledgeModelDTOChapters :: [FilledChapterDTO]
  , _filledKnowledgeModelDTOTags :: [TagDTO]
  , _filledKnowledgeModelDTOIntegrations :: [IntegrationDTO]
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data FilledChapterDTO = FilledChapterDTO
  { _filledChapterDTOUuid :: U.UUID
  , _filledChapterDTOHumanIdentifier :: String
  , _filledChapterDTOTitle :: String
  , _filledChapterDTOText :: String
  , _filledChapterDTOQuestions :: [FilledQuestionDTO]
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data FilledQuestionDTO
  = FilledOptionsQuestionDTO' FilledOptionsQuestionDTO
  | FilledListQuestionDTO' FilledListQuestionDTO
  | FilledValueQuestionDTO' FilledValueQuestionDTO
  | FilledIntegrationQuestionDTO' FilledIntegrationQuestionDTO
  deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

-- --------------------------------------------------------------------
data FilledAnswerDTO = FilledAnswerDTO
  { _filledAnswerDTOUuid :: U.UUID
  , _filledAnswerDTOHumanIdentifier :: String
  , _filledAnswerDTOLabel :: String
  , _filledAnswerDTOAdvice :: Maybe String
  , _filledAnswerDTOFollowUps :: [FilledQuestionDTO]
  , _filledAnswerDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data FilledAnswerItemDTO = FilledAnswerItemDTO
  { _filledAnswerItemDTOTitle :: String
  , _filledAnswerItemDTOHumanIdentifier :: String
  , _filledAnswerItemDTOValue :: Maybe String
  , _filledAnswerItemDTOQuestions :: [FilledQuestionDTO]
  } deriving (Show, Eq, Generic)
