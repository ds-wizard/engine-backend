module Wizard.Model.Project.Event.ProjectEventList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.ProjectReply
import WizardLib.Public.Model.User.UserSuggestion

data ProjectEventList
  = SetReplyEventList' SetReplyEventList
  | ClearReplyEventList' ClearReplyEventList
  | SetPhaseEventList' SetPhaseEventList
  | SetLabelsEventList' SetLabelsEventList
  deriving (Show, Eq, Generic)

data SetReplyEventList = SetReplyEventList
  { uuid :: U.UUID
  , path :: String
  , value :: ReplyValue
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetReplyEventList where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.value == b.value
      && a.createdBy == b.createdBy

data ClearReplyEventList = ClearReplyEventList
  { uuid :: U.UUID
  , path :: String
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ClearReplyEventList where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.createdBy == b.createdBy

data SetPhaseEventList = SetPhaseEventList
  { uuid :: U.UUID
  , phaseUuid :: Maybe U.UUID
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetPhaseEventList where
  a == b =
    a.uuid == b.uuid
      && a.phaseUuid == b.phaseUuid
      && a.createdBy == b.createdBy

data SetLabelsEventList = SetLabelsEventList
  { uuid :: U.UUID
  , path :: String
  , value :: [U.UUID]
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetLabelsEventList where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.value == b.value
      && a.createdBy == b.createdBy
