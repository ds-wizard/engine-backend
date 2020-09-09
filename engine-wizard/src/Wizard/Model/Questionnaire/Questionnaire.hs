module Wizard.Model.Questionnaire.Questionnaire where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireVisibility
  = PrivateQuestionnaire
  | VisibleViewQuestionnaire
  | VisibleEditQuestionnaire
  deriving (Show, Eq, Generic)

data QuestionnaireSharing
  = RestrictedQuestionnaire
  | AnyoneWithLinkViewQuestionnaire
  | AnyoneWithLinkEditQuestionnaire
  deriving (Show, Eq, Generic)

data Questionnaire =
  Questionnaire
    { _questionnaireUuid :: U.UUID
    , _questionnaireName :: String
    , _questionnaireLevel :: Int
    , _questionnaireVisibility :: QuestionnaireVisibility
    , _questionnaireSharing :: QuestionnaireSharing
    , _questionnairePackageId :: String
    , _questionnaireSelectedTagUuids :: [U.UUID]
    , _questionnaireTemplateId :: Maybe String
    , _questionnaireFormatUuid :: Maybe U.UUID
    , _questionnaireOwnerUuid :: Maybe U.UUID
    , _questionnaireCreatorUuid :: Maybe U.UUID
    , _questionnaireReplies :: M.Map String ReplyValue
    , _questionnaireLabels :: M.Map String [U.UUID]
    , _questionnaireCreatedAt :: UTCTime
    , _questionnaireUpdatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _questionnaireLevel a == _questionnaireLevel b &&
    _questionnaireVisibility a == _questionnaireVisibility b &&
    _questionnaireSharing a == _questionnaireSharing b &&
    _questionnairePackageId a == _questionnairePackageId b &&
    _questionnaireSelectedTagUuids a == _questionnaireSelectedTagUuids b &&
    _questionnaireTemplateId a == _questionnaireTemplateId b &&
    _questionnaireFormatUuid a == _questionnaireFormatUuid b &&
    _questionnaireOwnerUuid a == _questionnaireOwnerUuid b &&
    _questionnaireCreatorUuid a == _questionnaireCreatorUuid b &&
    _questionnaireReplies a == _questionnaireReplies b && _questionnaireLabels a == _questionnaireLabels b
