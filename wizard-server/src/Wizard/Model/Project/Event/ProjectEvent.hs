module Wizard.Model.Project.Event.ProjectEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.ProjectReply

data ProjectEvent
  = SetReplyEvent' SetReplyEvent
  | ClearReplyEvent' ClearReplyEvent
  | SetPhaseEvent' SetPhaseEvent
  | SetLabelsEvent' SetLabelsEvent
  deriving (Show, Eq, Generic)

data SetReplyEvent = SetReplyEvent
  { uuid :: U.UUID
  , path :: String
  , value :: ReplyValue
  , projectUuid :: U.UUID
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
      && a.projectUuid == b.projectUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy

data ClearReplyEvent = ClearReplyEvent
  { uuid :: U.UUID
  , path :: String
  , projectUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ClearReplyEvent where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.projectUuid == b.projectUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy

data SetPhaseEvent = SetPhaseEvent
  { uuid :: U.UUID
  , phaseUuid :: Maybe U.UUID
  , projectUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetPhaseEvent where
  a == b =
    a.uuid == b.uuid
      && a.phaseUuid == b.phaseUuid
      && a.projectUuid == b.projectUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy

data SetLabelsEvent = SetLabelsEvent
  { uuid :: U.UUID
  , path :: String
  , value :: [U.UUID]
  , projectUuid :: U.UUID
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
      && a.projectUuid == b.projectUuid
      && a.tenantUuid == b.tenantUuid
      && a.createdBy == b.createdBy
