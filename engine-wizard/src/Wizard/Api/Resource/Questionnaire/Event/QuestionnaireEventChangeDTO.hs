module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireEventChangeDTO
  = SetReplyEventChangeDTO' SetReplyEventChangeDTO
  | ClearReplyEventChangeDTO' ClearReplyEventChangeDTO
  | SetPhaseEventChangeDTO' SetPhaseEventChangeDTO
  | SetLabelsEventChangeDTO' SetLabelsEventChangeDTO
  | ResolveCommentThreadEventChangeDTO' ResolveCommentThreadEventChangeDTO
  | ReopenCommentThreadEventChangeDTO' ReopenCommentThreadEventChangeDTO
  | DeleteCommentThreadEventChangeDTO' DeleteCommentThreadEventChangeDTO
  | AddCommentEventChangeDTO' AddCommentEventChangeDTO
  | EditCommentEventChangeDTO' EditCommentEventChangeDTO
  | DeleteCommentEventChangeDTO' DeleteCommentEventChangeDTO
  deriving (Show, Eq, Generic)

data SetReplyEventChangeDTO =
  SetReplyEventChangeDTO
    { _setReplyEventChangeDTOUuid :: U.UUID
    , _setReplyEventChangeDTOPath :: String
    , _setReplyEventChangeDTOValue :: ReplyValue
    }
  deriving (Show, Eq, Generic)

data ClearReplyEventChangeDTO =
  ClearReplyEventChangeDTO
    { _clearReplyEventChangeDTOUuid :: U.UUID
    , _clearReplyEventChangeDTOPath :: String
    }
  deriving (Show, Eq, Generic)

data SetPhaseEventChangeDTO =
  SetPhaseEventChangeDTO
    { _setPhaseEventChangeDTOUuid :: U.UUID
    , _setPhaseEventChangeDTOPhaseUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)

data SetLabelsEventChangeDTO =
  SetLabelsEventChangeDTO
    { _setLabelsEventChangeDTOUuid :: U.UUID
    , _setLabelsEventChangeDTOPath :: String
    , _setLabelsEventChangeDTOValue :: [U.UUID]
    }
  deriving (Show, Eq, Generic)

data ResolveCommentThreadEventChangeDTO =
  ResolveCommentThreadEventChangeDTO
    { _resolveCommentThreadEventChangeDTOUuid :: U.UUID
    , _resolveCommentThreadEventChangeDTOThreadUuid :: U.UUID
    , _resolveCommentThreadEventChangeDTOPath :: String
    }
  deriving (Show, Eq, Generic)

data ReopenCommentThreadEventChangeDTO =
  ReopenCommentThreadEventChangeDTO
    { _reopenCommentThreadEventChangeDTOUuid :: U.UUID
    , _reopenCommentThreadEventChangeDTOThreadUuid :: U.UUID
    , _reopenCommentThreadEventChangeDTOPath :: String
    }
  deriving (Show, Eq, Generic)

data DeleteCommentThreadEventChangeDTO =
  DeleteCommentThreadEventChangeDTO
    { _deleteCommentThreadEventChangeDTOUuid :: U.UUID
    , _deleteCommentThreadEventChangeDTOPath :: String
    , _deleteCommentThreadEventChangeDTOThreadUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data AddCommentEventChangeDTO =
  AddCommentEventChangeDTO
    { _addCommentEventChangeDTOUuid :: U.UUID
    , _addCommentEventChangeDTOPath :: String
    , _addCommentEventChangeDTOThreadUuid :: U.UUID
    , _addCommentEventChangeDTOCommentUuid :: U.UUID
    , _addCommentEventChangeDTOText :: String
    }
  deriving (Show, Eq, Generic)

data EditCommentEventChangeDTO =
  EditCommentEventChangeDTO
    { _editCommentEventChangeDTOUuid :: U.UUID
    , _editCommentEventChangeDTOPath :: String
    , _editCommentEventChangeDTOThreadUuid :: U.UUID
    , _editCommentEventChangeDTOCommentUuid :: U.UUID
    , _editCommentEventChangeDTOText :: String
    }
  deriving (Show, Eq, Generic)

data DeleteCommentEventChangeDTO =
  DeleteCommentEventChangeDTO
    { _deleteCommentEventChangeDTOUuid :: U.UUID
    , _deleteCommentEventChangeDTOPath :: String
    , _deleteCommentEventChangeDTOThreadUuid :: U.UUID
    , _deleteCommentEventChangeDTOCommentUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
