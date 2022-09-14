module Wizard.Model.Questionnaire.Questionnaire where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireVersion

data QuestionnaireVisibility
  = PrivateQuestionnaire
  | VisibleViewQuestionnaire
  | VisibleCommentQuestionnaire
  | VisibleEditQuestionnaire
  deriving (Show, Eq, Generic, Read)

data QuestionnaireSharing
  = RestrictedQuestionnaire
  | AnyoneWithLinkViewQuestionnaire
  | AnyoneWithLinkCommentQuestionnaire
  | AnyoneWithLinkEditQuestionnaire
  deriving (Show, Eq, Generic, Read)

data Questionnaire =
  Questionnaire
    { _questionnaireUuid :: U.UUID
    , _questionnaireName :: String
    , _questionnaireDescription :: Maybe String
    , _questionnaireVisibility :: QuestionnaireVisibility
    , _questionnaireSharing :: QuestionnaireSharing
    , _questionnairePackageId :: String
    , _questionnaireSelectedQuestionTagUuids :: [U.UUID]
    , _questionnaireProjectTags :: [String]
    , _questionnaireTemplateId :: Maybe String
    , _questionnaireFormatUuid :: Maybe U.UUID
    , _questionnaireCreatorUuid :: Maybe U.UUID
    , _questionnairePermissions :: [QuestionnairePermRecord]
    , _questionnaireEvents :: [QuestionnaireEvent]
    , _questionnaireVersions :: [QuestionnaireVersion]
    , _questionnaireIsTemplate :: Bool
    , _questionnaireSquashed :: Bool
    , _questionnaireAnsweredQuestions :: Int
    , _questionnaireUnansweredQuestions :: Int
    , _questionnaireAppUuid :: U.UUID
    , _questionnaireCreatedAt :: UTCTime
    , _questionnaireUpdatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _questionnaireDescription a == _questionnaireDescription b &&
    _questionnaireVisibility a == _questionnaireVisibility b &&
    _questionnaireSharing a == _questionnaireSharing b &&
    _questionnairePackageId a == _questionnairePackageId b &&
    _questionnaireSelectedQuestionTagUuids a == _questionnaireSelectedQuestionTagUuids b &&
    _questionnaireProjectTags a == _questionnaireProjectTags b &&
    _questionnaireTemplateId a == _questionnaireTemplateId b &&
    _questionnaireFormatUuid a == _questionnaireFormatUuid b &&
    _questionnaireCreatorUuid a == _questionnaireCreatorUuid b &&
    _questionnairePermissions a == _questionnairePermissions b &&
    _questionnaireEvents a == _questionnaireEvents b &&
    _questionnaireVersions a == _questionnaireVersions b &&
    _questionnaireIsTemplate a == _questionnaireIsTemplate b &&
    _questionnaireSquashed a == _questionnaireSquashed b &&
    _questionnaireAnsweredQuestions a == _questionnaireAnsweredQuestions b &&
    _questionnaireUnansweredQuestions a == _questionnaireUnansweredQuestions b &&
    _questionnaireAppUuid a == _questionnaireAppUuid b
