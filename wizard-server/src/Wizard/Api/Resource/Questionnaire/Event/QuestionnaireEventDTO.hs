module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

data QuestionnaireEventDTO
  = SetReplyEventDTO' SetReplyEventDTO
  | ClearReplyEventDTO' ClearReplyEventDTO
  | SetPhaseEventDTO' SetPhaseEventDTO
  | SetLabelsEventDTO' SetLabelsEventDTO
  | ResolveCommentThreadEventDTO' ResolveCommentThreadEventDTO
  | ReopenCommentThreadEventDTO' ReopenCommentThreadEventDTO
  | DeleteCommentThreadEventDTO' DeleteCommentThreadEventDTO
  | AddCommentEventDTO' AddCommentEventDTO
  | EditCommentEventDTO' EditCommentEventDTO
  | DeleteCommentEventDTO' DeleteCommentEventDTO
  deriving (Show, Eq, Generic)

data SetReplyEventDTO = SetReplyEventDTO
  { uuid :: U.UUID
  , path :: String
  , value :: ReplyValue
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetReplyEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.value == b.value
      && a.createdBy == b.createdBy

data ClearReplyEventDTO = ClearReplyEventDTO
  { uuid :: U.UUID
  , path :: String
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ClearReplyEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.createdBy == b.createdBy

data SetPhaseEventDTO = SetPhaseEventDTO
  { uuid :: U.UUID
  , phaseUuid :: Maybe U.UUID
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetPhaseEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.phaseUuid == b.phaseUuid
      && a.createdBy == b.createdBy

data SetLabelsEventDTO = SetLabelsEventDTO
  { uuid :: U.UUID
  , path :: String
  , value :: [U.UUID]
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq SetLabelsEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.value == b.value
      && a.createdBy == b.createdBy

data ResolveCommentThreadEventDTO = ResolveCommentThreadEventDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ResolveCommentThreadEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.threadUuid == b.threadUuid
      && a.createdBy == b.createdBy

data ReopenCommentThreadEventDTO = ReopenCommentThreadEventDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentCount :: Int
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ReopenCommentThreadEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.threadUuid == b.threadUuid
      && a.createdBy == b.createdBy

data DeleteCommentThreadEventDTO = DeleteCommentThreadEventDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq DeleteCommentThreadEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.threadUuid == b.threadUuid
      && a.createdBy == b.createdBy

data AddCommentEventDTO = AddCommentEventDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentUuid :: U.UUID
  , text :: String
  , private :: Bool
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq AddCommentEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.threadUuid == b.threadUuid
      && a.commentUuid == b.commentUuid
      && a.text == b.text
      && a.createdBy == b.createdBy

data EditCommentEventDTO = EditCommentEventDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentUuid :: U.UUID
  , text :: String
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq EditCommentEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.threadUuid == b.threadUuid
      && a.commentUuid == b.commentUuid
      && a.text == b.text
      && a.createdBy == b.createdBy

data DeleteCommentEventDTO = DeleteCommentEventDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentUuid :: U.UUID
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq DeleteCommentEventDTO where
  a == b =
    a.uuid == b.uuid
      && a.path == b.path
      && a.threadUuid == b.threadUuid
      && a.commentUuid == b.commentUuid
      && a.createdBy == b.createdBy
