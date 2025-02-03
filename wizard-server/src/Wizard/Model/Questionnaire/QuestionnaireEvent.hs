module Wizard.Model.Questionnaire.QuestionnaireEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEventBundle = QuestionnaireEventBundle
  { events :: [QuestionnaireEvent]
  }
  deriving (Show, Generic)

data QuestionnaireEvent
  = SetReplyEvent' SetReplyEvent
  | ClearReplyEvent' ClearReplyEvent
  | SetPhaseEvent' SetPhaseEvent
  | SetLabelsEvent' SetLabelsEvent
  deriving (Show, Eq, Generic)

data SetReplyEvent = SetReplyEvent
  { uuid :: U.UUID
  , path :: String
  , value :: ReplyValue
  , questionnaireUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetReplyEvent where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.value == b.value
      && a.questionnaireUuid == b.questionnaireUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy

data ClearReplyEvent = ClearReplyEvent
  { uuid :: U.UUID
  , path :: String
  , questionnaireUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ClearReplyEvent where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.questionnaireUuid == b.questionnaireUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy

data SetPhaseEvent = SetPhaseEvent
  { uuid :: U.UUID
  , phaseUuid :: Maybe U.UUID
  , questionnaireUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetPhaseEvent where
  a == b =
    a.uuid == b.uuid
      && a.phaseUuid == b.phaseUuid
      && a.questionnaireUuid == b.questionnaireUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy

data SetLabelsEvent = SetLabelsEvent
  { uuid :: U.UUID
  , path :: String
  , value :: [U.UUID]
  , questionnaireUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetLabelsEvent where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.value == b.value
      && a.questionnaireUuid == b.questionnaireUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy
