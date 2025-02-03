module Wizard.Model.Questionnaire.Questionnaire where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnairePerm
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

data Questionnaire = Questionnaire
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , packageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , projectTags :: [String]
  , documentTemplateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  , creatorUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePerm]
  , versions :: [QuestionnaireVersion]
  , isTemplate :: Bool
  , squashed :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.packageId == b.packageId
      && a.selectedQuestionTagUuids == b.selectedQuestionTagUuids
      && a.projectTags == b.projectTags
      && a.documentTemplateId == b.documentTemplateId
      && a.formatUuid == b.formatUuid
      && a.creatorUuid == b.creatorUuid
      && a.permissions == b.permissions
      && a.versions == b.versions
      && a.isTemplate == b.isTemplate
      && a.squashed == b.squashed
      && a.tenantUuid == b.tenantUuid
