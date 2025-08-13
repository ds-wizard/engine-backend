module WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel where

import Data.Map
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry

type KMParentMap = M.Map U.UUID U.UUID

data KnowledgeModel = KnowledgeModel
  { uuid :: U.UUID
  , annotations :: [MapEntry String String]
  , chapterUuids :: [U.UUID]
  , tagUuids :: [U.UUID]
  , integrationUuids :: [U.UUID]
  , metricUuids :: [U.UUID]
  , phaseUuids :: [U.UUID]
  , resourceCollectionUuids :: [U.UUID]
  , entities :: KnowledgeModelEntities
  }
  deriving (Show, Eq, Generic)

data KnowledgeModelEntities = KnowledgeModelEntities
  { chapters :: Map U.UUID Chapter
  , questions :: Map U.UUID Question
  , answers :: Map U.UUID Answer
  , choices :: Map U.UUID Choice
  , experts :: Map U.UUID Expert
  , references :: Map U.UUID Reference
  , integrations :: Map U.UUID Integration
  , tags :: Map U.UUID Tag
  , metrics :: Map U.UUID Metric
  , resourceCollections :: Map U.UUID ResourceCollection
  , resourcePages :: Map U.UUID ResourcePage
  , phases :: Map U.UUID Phase
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Chapter = Chapter
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , annotations :: [MapEntry String String]
  , questionUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | DateTimeQuestionValueType
  | TimeQuestionValueType
  | TextQuestionValueType
  | EmailQuestionValueType
  | UrlQuestionValueType
  | ColorQuestionValueType
  deriving (Show, Eq, Generic, Read)

data Question
  = OptionsQuestion' OptionsQuestion
  | MultiChoiceQuestion' MultiChoiceQuestion
  | ListQuestion' ListQuestion
  | ValueQuestion' ValueQuestion
  | IntegrationQuestion' IntegrationQuestion
  | ItemSelectQuestion' ItemSelectQuestion
  | FileQuestion' FileQuestion
  deriving (Show, Eq, Generic)

data OptionsQuestion = OptionsQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , answerUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

data MultiChoiceQuestion = MultiChoiceQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , choiceUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

data ListQuestion = ListQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , itemTemplateQuestionUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

data ValueQuestion = ValueQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , valueType :: QuestionValueType
  , validations :: [QuestionValidation]
  }
  deriving (Show, Eq, Generic)

data QuestionValidation
  = MinLengthQuestionValidation {iValue :: Int}
  | MaxLengthQuestionValidation {iValue :: Int}
  | RegexQuestionValidation {sValue :: String}
  | OrcidQuestionValidation
  | DoiQuestionValidation
  | MinNumberQuestionValidation {dValue :: Double}
  | MaxNumberQuestionValidation {dValue :: Double}
  | FromDateQuestionValidation {sValue :: String}
  | ToDateQuestionValidation {sValue :: String}
  | FromDateTimeQuestionValidation {sValue :: String}
  | ToDateTimeQuestionValidation {sValue :: String}
  | FromTimeQuestionValidation {sValue :: String}
  | ToTimeQuestionValidation {sValue :: String}
  | DomainQuestionValidation {sValue :: String}
  deriving (Show, Eq, Generic)

data IntegrationQuestion = IntegrationQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , integrationUuid :: U.UUID
  , props :: Map String String
  }
  deriving (Show, Eq, Generic)

data ItemSelectQuestion = ItemSelectQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , listQuestionUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

data FileQuestion = FileQuestion
  { uuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , requiredPhaseUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  , tagUuids :: [U.UUID]
  , expertUuids :: [U.UUID]
  , referenceUuids :: [U.UUID]
  , maxSize :: Maybe Int
  , fileTypes :: Maybe String
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Answer = Answer
  { uuid :: U.UUID
  , aLabel :: String
  , advice :: Maybe String
  , annotations :: [MapEntry String String]
  , followUpUuids :: [U.UUID]
  , metricMeasures :: [MetricMeasure]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Choice = Choice
  { uuid :: U.UUID
  , aLabel :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Expert = Expert
  { uuid :: U.UUID
  , name :: String
  , email :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Reference
  = ResourcePageReference' ResourcePageReference
  | URLReference' URLReference
  | CrossReference' CrossReference
  deriving (Show, Eq, Generic)

data ResourcePageReference = ResourcePageReference
  { uuid :: U.UUID
  , resourcePageUuid :: Maybe U.UUID
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

data URLReference = URLReference
  { uuid :: U.UUID
  , url :: String
  , aLabel :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

data CrossReference = CrossReference
  { uuid :: U.UUID
  , targetUuid :: U.UUID
  , description :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Metric = Metric
  { uuid :: U.UUID
  , title :: String
  , abbreviation :: Maybe String
  , description :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

data MetricMeasure = MetricMeasure
  { metricUuid :: U.UUID
  , measure :: Double
  , weight :: Double
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Phase = Phase
  { uuid :: U.UUID
  , title :: String
  , description :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Tag = Tag
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , color :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data ResourceCollection = ResourceCollection
  { uuid :: U.UUID
  , title :: String
  , resourcePageUuids :: [U.UUID]
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

data ResourcePage = ResourcePage
  { uuid :: U.UUID
  , title :: String
  , content :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

-- ------------------------------------------------
data Integration
  = ApiIntegration' ApiIntegration
  | ApiLegacyIntegration' ApiLegacyIntegration
  | WidgetIntegration' WidgetIntegration
  deriving (Show, Eq, Generic)

data ApiIntegration = ApiIntegration
  { uuid :: U.UUID
  , name :: String
  , variables :: [String]
  , allowCustomReply :: Bool
  , requestMethod :: String
  , requestUrl :: String
  , requestHeaders :: [MapEntry String String]
  , requestBody :: Maybe String
  , requestAllowEmptySearch :: Bool
  , responseListField :: Maybe String
  , responseItemTemplate :: String
  , responseItemTemplateForSelection :: Maybe String
  , testQ :: String
  , testVariables :: M.Map String String
  , testResponse :: Maybe TypeHintExchange
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

data TypeHintExchange = TypeHintExchange
  { request :: TypeHintRequest
  , response :: TypeHintResponse
  }
  deriving (Show, Eq, Generic)

data TypeHintRequest = TypeHintRequest
  { method :: String
  , url :: String
  , headers :: [MapEntry String String]
  , body :: Maybe String
  }
  deriving (Show, Eq, Generic)

data TypeHintResponse
  = SuccessTypeHintResponse' SuccessTypeHintResponse
  | RemoteErrorTypeHintResponse' RemoteErrorTypeHintResponse
  | RequestFailedTypeHintResponse' RequestFailedTypeHintResponse
  deriving (Show, Eq, Generic)

data SuccessTypeHintResponse = SuccessTypeHintResponse
  { status :: Int
  , contentType :: Maybe String
  , body :: String
  }
  deriving (Show, Eq, Generic)

data RemoteErrorTypeHintResponse = RemoteErrorTypeHintResponse
  { status :: Int
  , contentType :: Maybe String
  , body :: String
  }
  deriving (Show, Eq, Generic)

data RequestFailedTypeHintResponse = RequestFailedTypeHintResponse
  { message :: String
  }
  deriving (Show, Eq, Generic)

data ApiLegacyIntegration = ApiLegacyIntegration
  { uuid :: U.UUID
  , iId :: String
  , name :: String
  , props :: [String]
  , logo :: Maybe String
  , requestMethod :: String
  , requestUrl :: String
  , requestHeaders :: [MapEntry String String]
  , requestBody :: String
  , requestEmptySearch :: Bool
  , responseListField :: Maybe String
  , responseItemId :: Maybe String
  , responseItemTemplate :: String
  , itemUrl :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

data WidgetIntegration = WidgetIntegration
  { uuid :: U.UUID
  , iId :: String
  , name :: String
  , props :: [String]
  , logo :: Maybe String
  , widgetUrl :: String
  , itemUrl :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)
