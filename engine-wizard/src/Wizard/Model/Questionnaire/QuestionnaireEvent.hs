module Wizard.Model.Questionnaire.QuestionnaireEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEventBundle =
  QuestionnaireEventBundle
    { _questionnaireEventBundleEvents :: [QuestionnaireEvent]
    }
  deriving (Show, Generic)

data QuestionnaireEvent
  = SetReplyEvent' SetReplyEvent
  | ClearReplyEvent' ClearReplyEvent
  | SetPhaseEvent' SetPhaseEvent
  | SetLabelsEvent' SetLabelsEvent
  | ResolveCommentThreadEvent' ResolveCommentThreadEvent
  | ReopenCommentThreadEvent' ReopenCommentThreadEvent
  | DeleteCommentThreadEvent' DeleteCommentThreadEvent
  | AddCommentEvent' AddCommentEvent
  | EditCommentEvent' EditCommentEvent
  | DeleteCommentEvent' DeleteCommentEvent
  deriving (Show, Eq, Generic)

data SetReplyEvent =
  SetReplyEvent
    { _setReplyEventUuid :: U.UUID
    , _setReplyEventPath :: String
    , _setReplyEventValue :: ReplyValue
    , _setReplyEventCreatedBy :: Maybe U.UUID
    , _setReplyEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetReplyEvent where
  a == b =
    _setReplyEventUuid a == _setReplyEventUuid b &&
    _setReplyEventPath a == _setReplyEventPath b &&
    _setReplyEventValue a == _setReplyEventValue b && _setReplyEventCreatedBy a == _setReplyEventCreatedBy b

data ClearReplyEvent =
  ClearReplyEvent
    { _clearReplyEventUuid :: U.UUID
    , _clearReplyEventPath :: String
    , _clearReplyEventCreatedBy :: Maybe U.UUID
    , _clearReplyEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ClearReplyEvent where
  a == b =
    _clearReplyEventUuid a == _clearReplyEventUuid b &&
    _clearReplyEventPath a == _clearReplyEventPath b && _clearReplyEventCreatedBy a == _clearReplyEventCreatedBy b

data SetPhaseEvent =
  SetPhaseEvent
    { _setPhaseEventUuid :: U.UUID
    , _setPhaseEventPhaseUuid :: Maybe U.UUID
    , _setPhaseEventCreatedBy :: Maybe U.UUID
    , _setPhaseEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetPhaseEvent where
  a == b =
    _setPhaseEventUuid a == _setPhaseEventUuid b &&
    _setPhaseEventPhaseUuid a == _setPhaseEventPhaseUuid b && _setPhaseEventCreatedBy a == _setPhaseEventCreatedBy b

data SetLabelsEvent =
  SetLabelsEvent
    { _setLabelsEventUuid :: U.UUID
    , _setLabelsEventPath :: String
    , _setLabelsEventValue :: [U.UUID]
    , _setLabelsEventCreatedBy :: Maybe U.UUID
    , _setLabelsEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq SetLabelsEvent where
  a == b =
    _setLabelsEventUuid a == _setLabelsEventUuid b &&
    _setLabelsEventPath a == _setLabelsEventPath b &&
    _setLabelsEventValue a == _setLabelsEventValue b && _setLabelsEventCreatedBy a == _setLabelsEventCreatedBy b

data ResolveCommentThreadEvent =
  ResolveCommentThreadEvent
    { _resolveCommentThreadEventUuid :: U.UUID
    , _resolveCommentThreadEventPath :: String
    , _resolveCommentThreadEventThreadUuid :: U.UUID
    , _resolveCommentThreadEventCreatedBy :: Maybe U.UUID
    , _resolveCommentThreadEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ResolveCommentThreadEvent where
  a == b =
    _resolveCommentThreadEventUuid a == _resolveCommentThreadEventUuid b &&
    _resolveCommentThreadEventPath a == _resolveCommentThreadEventPath b &&
    _resolveCommentThreadEventThreadUuid a == _resolveCommentThreadEventThreadUuid b &&
    _resolveCommentThreadEventCreatedBy a == _resolveCommentThreadEventCreatedBy b

data ReopenCommentThreadEvent =
  ReopenCommentThreadEvent
    { _reopenCommentThreadEventUuid :: U.UUID
    , _reopenCommentThreadEventPath :: String
    , _reopenCommentThreadEventThreadUuid :: U.UUID
    , _reopenCommentThreadEventCreatedBy :: Maybe U.UUID
    , _reopenCommentThreadEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ReopenCommentThreadEvent where
  a == b =
    _reopenCommentThreadEventUuid a == _reopenCommentThreadEventUuid b &&
    _reopenCommentThreadEventPath a == _reopenCommentThreadEventPath b &&
    _reopenCommentThreadEventThreadUuid a == _reopenCommentThreadEventThreadUuid b &&
    _reopenCommentThreadEventCreatedBy a == _reopenCommentThreadEventCreatedBy b

data DeleteCommentThreadEvent =
  DeleteCommentThreadEvent
    { _deleteCommentThreadEventUuid :: U.UUID
    , _deleteCommentThreadEventPath :: String
    , _deleteCommentThreadEventThreadUuid :: U.UUID
    , _deleteCommentThreadEventCreatedBy :: Maybe U.UUID
    , _deleteCommentThreadEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DeleteCommentThreadEvent where
  a == b =
    _deleteCommentThreadEventUuid a == _deleteCommentThreadEventUuid b &&
    _deleteCommentThreadEventPath a == _deleteCommentThreadEventPath b &&
    _deleteCommentThreadEventThreadUuid a == _deleteCommentThreadEventThreadUuid b &&
    _deleteCommentThreadEventCreatedBy a == _deleteCommentThreadEventCreatedBy b

data AddCommentEvent =
  AddCommentEvent
    { _addCommentEventUuid :: U.UUID
    , _addCommentEventThreadUuid :: U.UUID
    , _addCommentEventCommentUuid :: U.UUID
    , _addCommentEventPath :: String
    , _addCommentEventText :: String
    , _addCommentEventPrivate :: Bool
    , _addCommentEventCreatedBy :: Maybe U.UUID
    , _addCommentEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq AddCommentEvent where
  a == b =
    _addCommentEventUuid a == _addCommentEventUuid b &&
    _addCommentEventPath a == _addCommentEventPath b &&
    _addCommentEventText a == _addCommentEventText b &&
    _addCommentEventCreatedBy a == _addCommentEventCreatedBy b && _addCommentEventPrivate a == _addCommentEventPrivate b

data EditCommentEvent =
  EditCommentEvent
    { _editCommentEventUuid :: U.UUID
    , _editCommentEventPath :: String
    , _editCommentEventThreadUuid :: U.UUID
    , _editCommentEventCommentUuid :: U.UUID
    , _editCommentEventText :: String
    , _editCommentEventCreatedBy :: Maybe U.UUID
    , _editCommentEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq EditCommentEvent where
  a == b =
    _editCommentEventUuid a == _editCommentEventUuid b &&
    _editCommentEventPath a == _editCommentEventPath b &&
    _editCommentEventThreadUuid a == _editCommentEventThreadUuid b &&
    _editCommentEventCommentUuid a == _editCommentEventCommentUuid b &&
    _editCommentEventText a == _editCommentEventText b && _editCommentEventCreatedBy a == _editCommentEventCreatedBy b

data DeleteCommentEvent =
  DeleteCommentEvent
    { _deleteCommentEventUuid :: U.UUID
    , _deleteCommentEventPath :: String
    , _deleteCommentEventThreadUuid :: U.UUID
    , _deleteCommentEventCommentUuid :: U.UUID
    , _deleteCommentEventCreatedBy :: Maybe U.UUID
    , _deleteCommentEventCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DeleteCommentEvent where
  a == b =
    _deleteCommentEventUuid a == _deleteCommentEventUuid b &&
    _deleteCommentEventPath a == _deleteCommentEventPath b &&
    _deleteCommentEventThreadUuid a == _deleteCommentEventThreadUuid b &&
    _deleteCommentEventCommentUuid a == _deleteCommentEventCommentUuid b &&
    _deleteCommentEventCreatedBy a == _deleteCommentEventCreatedBy b
