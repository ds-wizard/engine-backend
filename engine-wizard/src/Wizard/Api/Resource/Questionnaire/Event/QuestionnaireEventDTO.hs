module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.Questionnaire.QuestionnaireReply

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

data SetReplyEventDTO =
  SetReplyEventDTO
    { _setReplyEventDTOUuid :: U.UUID
    , _setReplyEventDTOPath :: String
    , _setReplyEventDTOValue :: ReplyValue
    , _setReplyEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _setReplyEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetReplyEventDTO where
  a == b =
    _setReplyEventDTOUuid a == _setReplyEventDTOUuid b &&
    _setReplyEventDTOPath a == _setReplyEventDTOPath b &&
    _setReplyEventDTOValue a == _setReplyEventDTOValue b && _setReplyEventDTOCreatedBy a == _setReplyEventDTOCreatedBy b

data ClearReplyEventDTO =
  ClearReplyEventDTO
    { _clearReplyEventDTOUuid :: U.UUID
    , _clearReplyEventDTOPath :: String
    , _clearReplyEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _clearReplyEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ClearReplyEventDTO where
  a == b =
    _clearReplyEventDTOUuid a == _clearReplyEventDTOUuid b &&
    _clearReplyEventDTOPath a == _clearReplyEventDTOPath b &&
    _clearReplyEventDTOCreatedBy a == _clearReplyEventDTOCreatedBy b

data SetPhaseEventDTO =
  SetPhaseEventDTO
    { _setPhaseEventDTOUuid :: U.UUID
    , _setPhaseEventDTOPhaseUuid :: Maybe U.UUID
    , _setPhaseEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _setPhaseEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetPhaseEventDTO where
  a == b =
    _setPhaseEventDTOUuid a == _setPhaseEventDTOUuid b &&
    _setPhaseEventDTOPhaseUuid a == _setPhaseEventDTOPhaseUuid b &&
    _setPhaseEventDTOCreatedBy a == _setPhaseEventDTOCreatedBy b

data SetLabelsEventDTO =
  SetLabelsEventDTO
    { _setLabelsEventDTOUuid :: U.UUID
    , _setLabelsEventDTOPath :: String
    , _setLabelsEventDTOValue :: [U.UUID]
    , _setLabelsEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _setLabelsEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetLabelsEventDTO where
  a == b =
    _setLabelsEventDTOUuid a == _setLabelsEventDTOUuid b &&
    _setLabelsEventDTOPath a == _setLabelsEventDTOPath b &&
    _setLabelsEventDTOValue a == _setLabelsEventDTOValue b &&
    _setLabelsEventDTOCreatedBy a == _setLabelsEventDTOCreatedBy b

data ResolveCommentThreadEventDTO =
  ResolveCommentThreadEventDTO
    { _resolveCommentThreadEventDTOUuid :: U.UUID
    , _resolveCommentThreadEventDTOPath :: String
    , _resolveCommentThreadEventDTOThreadUuid :: U.UUID
    , _resolveCommentThreadEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _resolveCommentThreadEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ResolveCommentThreadEventDTO where
  a == b =
    _resolveCommentThreadEventDTOUuid a == _resolveCommentThreadEventDTOUuid b &&
    _resolveCommentThreadEventDTOPath a == _resolveCommentThreadEventDTOPath b &&
    _resolveCommentThreadEventDTOThreadUuid a == _resolveCommentThreadEventDTOThreadUuid b &&
    _resolveCommentThreadEventDTOCreatedBy a == _resolveCommentThreadEventDTOCreatedBy b

data ReopenCommentThreadEventDTO =
  ReopenCommentThreadEventDTO
    { _reopenCommentThreadEventDTOUuid :: U.UUID
    , _reopenCommentThreadEventDTOPath :: String
    , _reopenCommentThreadEventDTOThreadUuid :: U.UUID
    , _reopenCommentThreadEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _reopenCommentThreadEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ReopenCommentThreadEventDTO where
  a == b =
    _reopenCommentThreadEventDTOUuid a == _reopenCommentThreadEventDTOUuid b &&
    _reopenCommentThreadEventDTOPath a == _reopenCommentThreadEventDTOPath b &&
    _reopenCommentThreadEventDTOThreadUuid a == _reopenCommentThreadEventDTOThreadUuid b &&
    _reopenCommentThreadEventDTOCreatedBy a == _reopenCommentThreadEventDTOCreatedBy b

data DeleteCommentThreadEventDTO =
  DeleteCommentThreadEventDTO
    { _deleteCommentThreadEventDTOUuid :: U.UUID
    , _deleteCommentThreadEventDTOPath :: String
    , _deleteCommentThreadEventDTOThreadUuid :: U.UUID
    , _deleteCommentThreadEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _deleteCommentThreadEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DeleteCommentThreadEventDTO where
  a == b =
    _deleteCommentThreadEventDTOUuid a == _deleteCommentThreadEventDTOUuid b &&
    _deleteCommentThreadEventDTOPath a == _deleteCommentThreadEventDTOPath b &&
    _deleteCommentThreadEventDTOThreadUuid a == _deleteCommentThreadEventDTOThreadUuid b &&
    _deleteCommentThreadEventDTOCreatedBy a == _deleteCommentThreadEventDTOCreatedBy b

data AddCommentEventDTO =
  AddCommentEventDTO
    { _addCommentEventDTOUuid :: U.UUID
    , _addCommentEventDTOPath :: String
    , _addCommentEventDTOThreadUuid :: U.UUID
    , _addCommentEventDTOCommentUuid :: U.UUID
    , _addCommentEventDTOText :: String
    , _addCommentEventDTOPrivate :: Bool
    , _addCommentEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _addCommentEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq AddCommentEventDTO where
  a == b =
    _addCommentEventDTOUuid a == _addCommentEventDTOUuid b &&
    _addCommentEventDTOPath a == _addCommentEventDTOPath b &&
    _addCommentEventDTOThreadUuid a == _addCommentEventDTOThreadUuid b &&
    _addCommentEventDTOCommentUuid a == _addCommentEventDTOCommentUuid b &&
    _addCommentEventDTOText a == _addCommentEventDTOText b &&
    _addCommentEventDTOCreatedBy a == _addCommentEventDTOCreatedBy b

data EditCommentEventDTO =
  EditCommentEventDTO
    { _editCommentEventDTOUuid :: U.UUID
    , _editCommentEventDTOPath :: String
    , _editCommentEventDTOThreadUuid :: U.UUID
    , _editCommentEventDTOCommentUuid :: U.UUID
    , _editCommentEventDTOText :: String
    , _editCommentEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _editCommentEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq EditCommentEventDTO where
  a == b =
    _editCommentEventDTOUuid a == _editCommentEventDTOUuid b &&
    _editCommentEventDTOPath a == _editCommentEventDTOPath b &&
    _editCommentEventDTOThreadUuid a == _editCommentEventDTOThreadUuid b &&
    _editCommentEventDTOCommentUuid a == _editCommentEventDTOCommentUuid b &&
    _editCommentEventDTOText a == _editCommentEventDTOText b &&
    _editCommentEventDTOCreatedBy a == _editCommentEventDTOCreatedBy b

data DeleteCommentEventDTO =
  DeleteCommentEventDTO
    { _deleteCommentEventDTOUuid :: U.UUID
    , _deleteCommentEventDTOPath :: String
    , _deleteCommentEventDTOThreadUuid :: U.UUID
    , _deleteCommentEventDTOCommentUuid :: U.UUID
    , _deleteCommentEventDTOCreatedBy :: Maybe UserSuggestionDTO
    , _deleteCommentEventDTOCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DeleteCommentEventDTO where
  a == b =
    _deleteCommentEventDTOUuid a == _deleteCommentEventDTOUuid b &&
    _deleteCommentEventDTOPath a == _deleteCommentEventDTOPath b &&
    _deleteCommentEventDTOThreadUuid a == _deleteCommentEventDTOThreadUuid b &&
    _deleteCommentEventDTOCommentUuid a == _deleteCommentEventDTOCommentUuid b &&
    _deleteCommentEventDTOCreatedBy a == _deleteCommentEventDTOCreatedBy b
