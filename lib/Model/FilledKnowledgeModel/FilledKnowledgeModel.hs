module Model.FilledKnowledgeModel.FilledKnowledgeModel where

import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

data FilledKnowledgeModel = FilledKnowledgeModel
  { _filledKnowledgeModelUuid :: U.UUID
  , _filledKnowledgeModelName :: String
  , _filledKnowledgeModelChapters :: [FilledChapter]
  , _filledKnowledgeModelTags :: [Tag]
  , _filledKnowledgeModelIntegrations :: [Integration]
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data FilledChapter = FilledChapter
  { _filledChapterUuid :: U.UUID
  , _filledChapterHumanIdentifier :: String
  , _filledChapterTitle :: String
  , _filledChapterText :: String
  , _filledChapterQuestions :: [FilledQuestion]
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data FilledQuestion
  = FilledOptionsQuestion' FilledOptionsQuestion
  | FilledListQuestion' FilledListQuestion
  | FilledValueQuestion' FilledValueQuestion
  | FilledIntegrationQuestion' FilledIntegrationQuestion
  deriving (Show, Eq, Generic)

data FilledOptionsQuestion = FilledOptionsQuestion
  { _filledOptionsQuestionUuid :: U.UUID
  , _filledOptionsQuestionHumanIdentifier :: String
  , _filledOptionsQuestionTitle :: String
  , _filledOptionsQuestionText :: Maybe String
  , _filledOptionsQuestionRequiredLevel :: Maybe Int
  , _filledOptionsQuestionTagUuids :: [U.UUID]
  , _filledOptionsQuestionExperts :: [Expert]
  , _filledOptionsQuestionReferences :: [Reference]
  , _filledOptionsQuestionAnswers :: [Answer]
  , _filledOptionsQuestionAnswerOption :: Maybe FilledAnswer
  } deriving (Show, Eq, Generic)

data FilledListQuestion = FilledListQuestion
  { _filledListQuestionUuid :: U.UUID
  , _filledListQuestionHumanIdentifier :: String
  , _filledListQuestionTitle :: String
  , _filledListQuestionText :: Maybe String
  , _filledListQuestionRequiredLevel :: Maybe Int
  , _filledListQuestionTagUuids :: [U.UUID]
  , _filledListQuestionExperts :: [Expert]
  , _filledListQuestionReferences :: [Reference]
  , _filledListQuestionItemTemplateTitle :: String
  , _filledListQuestionItemTemplateQuestions :: [Question]
  , _filledListQuestionItems :: Maybe [FilledAnswerItem]
  } deriving (Show, Eq, Generic)

data FilledValueQuestion = FilledValueQuestion
  { _filledValueQuestionUuid :: U.UUID
  , _filledValueQuestionHumanIdentifier :: String
  , _filledValueQuestionTitle :: String
  , _filledValueQuestionText :: Maybe String
  , _filledValueQuestionRequiredLevel :: Maybe Int
  , _filledValueQuestionTagUuids :: [U.UUID]
  , _filledValueQuestionExperts :: [Expert]
  , _filledValueQuestionReferences :: [Reference]
  , _filledValueQuestionValueType :: QuestionValueType
  , _filledValueQuestionAnswerValue :: Maybe String
  } deriving (Show, Eq, Generic)

data FilledIntegrationQuestion = FilledIntegrationQuestion
  { _filledIntegrationQuestionUuid :: U.UUID
  , _filledIntegrationQuestionHumanIdentifier :: String
  , _filledIntegrationQuestionTitle :: String
  , _filledIntegrationQuestionText :: Maybe String
  , _filledIntegrationQuestionRequiredLevel :: Maybe Int
  , _filledIntegrationQuestionTagUuids :: [U.UUID]
  , _filledIntegrationQuestionExperts :: [Expert]
  , _filledIntegrationQuestionReferences :: [Reference]
  , _filledIntegrationQuestionIntegrationUuid :: U.UUID
  , _filledIntegrationQuestionProps :: Map String String
  , _filledIntegrationQuestionAnswerIntId :: Maybe String
  , _filledIntegrationQuestionAnswerValue :: Maybe String
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data FilledAnswer = FilledAnswer
  { _filledAnswerUuid :: U.UUID
  , _filledAnswerHumanIdentifier :: String
  , _filledAnswerLabel :: String
  , _filledAnswerAdvice :: Maybe String
  , _filledAnswerFollowUps :: [FilledQuestion]
  , _filledAnswerMetricMeasures :: [MetricMeasure]
  } deriving (Show, Eq, Generic)

-- ------------------------------------------------
data FilledAnswerItem = FilledAnswerItem
  { _filledAnswerItemTitle :: String
  , _filledAnswerItemHumanIdentifier :: String
  , _filledAnswerItemValue :: Maybe String
  , _filledAnswerItemQuestions :: [FilledQuestion]
  } deriving (Show, Eq, Generic)
