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
  | VisibleEditQuestionnaire
  deriving (Show, Eq, Generic, Read)

data QuestionnaireSharing
  = RestrictedQuestionnaire
  | AnyoneWithLinkViewQuestionnaire
  | AnyoneWithLinkEditQuestionnaire
  deriving (Show, Eq, Generic, Read)

data Questionnaire =
  Questionnaire
    { _questionnaireUuid :: U.UUID
    , _questionnaireName :: String
    , _questionnaireVisibility :: QuestionnaireVisibility
    , _questionnaireSharing :: QuestionnaireSharing
    , _questionnairePackageId :: String
    , _questionnaireSelectedTagUuids :: [U.UUID]
    , _questionnaireTemplateId :: Maybe String
    , _questionnaireFormatUuid :: Maybe U.UUID
    , _questionnaireCreatorUuid :: Maybe U.UUID
    , _questionnairePermissions :: [QuestionnairePermRecord]
    , _questionnaireEvents :: [QuestionnaireEvent]
    , _questionnaireVersions :: [QuestionnaireVersion]
    , _questionnaireCreatedAt :: UTCTime
    , _questionnaireUpdatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _questionnaireVisibility a == _questionnaireVisibility b &&
    _questionnaireSharing a == _questionnaireSharing b &&
    _questionnairePackageId a == _questionnairePackageId b &&
    _questionnaireSelectedTagUuids a == _questionnaireSelectedTagUuids b &&
    _questionnaireTemplateId a == _questionnaireTemplateId b &&
    _questionnaireFormatUuid a == _questionnaireFormatUuid b &&
    _questionnaireCreatorUuid a == _questionnaireCreatorUuid b &&
    _questionnairePermissions a == _questionnairePermissions b &&
    _questionnaireEvents a == _questionnaireEvents b && _questionnaireVersions a == _questionnaireVersions b
