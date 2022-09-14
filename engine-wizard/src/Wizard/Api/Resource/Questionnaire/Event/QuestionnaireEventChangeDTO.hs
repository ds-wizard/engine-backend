module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report

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
    , _setReplyEventChangeDTOPhasesAnsweredIndication :: PhasesAnsweredIndication
    }
  deriving (Show, Eq, Generic)

data ClearReplyEventChangeDTO =
  ClearReplyEventChangeDTO
    { _clearReplyEventChangeDTOUuid :: U.UUID
    , _clearReplyEventChangeDTOPath :: String
    , _clearReplyEventChangeDTOPhasesAnsweredIndication :: PhasesAnsweredIndication
    }
  deriving (Show, Eq, Generic)

data SetPhaseEventChangeDTO =
  SetPhaseEventChangeDTO
    { _setPhaseEventChangeDTOUuid :: U.UUID
    , _setPhaseEventChangeDTOPhaseUuid :: Maybe U.UUID
    , _setPhaseEventChangeDTOPhasesAnsweredIndication :: PhasesAnsweredIndication
    }
  deriving (Show, Eq, Generic)

data SetLabelsEventChangeDTO =
  SetLabelsEventChangeDTO
    { _setLabelsEventChangeDTOUuid :: U.UUID
    , _setLabelsEventChangeDTOPath :: String
    , _setLabelsEventChangeDTOValue :: [U.UUID]
    , _setLabelsEventChangeDTOPhasesAnsweredIndication :: PhasesAnsweredIndication
    }
  deriving (Show, Eq, Generic)

data ResolveCommentThreadEventChangeDTO =
  ResolveCommentThreadEventChangeDTO
    { _resolveCommentThreadEventChangeDTOUuid :: U.UUID
    , _resolveCommentThreadEventChangeDTOThreadUuid :: U.UUID
    , _resolveCommentThreadEventChangeDTOPath :: String
    , _resolveCommentThreadEventChangeDTOPrivate :: Bool
    }
  deriving (Show, Eq, Generic)

data ReopenCommentThreadEventChangeDTO =
  ReopenCommentThreadEventChangeDTO
    { _reopenCommentThreadEventChangeDTOUuid :: U.UUID
    , _reopenCommentThreadEventChangeDTOThreadUuid :: U.UUID
    , _reopenCommentThreadEventChangeDTOPath :: String
    , _reopenCommentThreadEventChangeDTOPrivate :: Bool
    }
  deriving (Show, Eq, Generic)

data DeleteCommentThreadEventChangeDTO =
  DeleteCommentThreadEventChangeDTO
    { _deleteCommentThreadEventChangeDTOUuid :: U.UUID
    , _deleteCommentThreadEventChangeDTOPath :: String
    , _deleteCommentThreadEventChangeDTOThreadUuid :: U.UUID
    , _deleteCommentThreadEventChangeDTOPrivate :: Bool
    }
  deriving (Show, Eq, Generic)

data AddCommentEventChangeDTO =
  AddCommentEventChangeDTO
    { _addCommentEventChangeDTOUuid :: U.UUID
    , _addCommentEventChangeDTOPath :: String
    , _addCommentEventChangeDTOThreadUuid :: U.UUID
    , _addCommentEventChangeDTOCommentUuid :: U.UUID
    , _addCommentEventChangeDTOText :: String
    , _addCommentEventChangeDTOPrivate :: Bool
    }
  deriving (Show, Eq, Generic)

data EditCommentEventChangeDTO =
  EditCommentEventChangeDTO
    { _editCommentEventChangeDTOUuid :: U.UUID
    , _editCommentEventChangeDTOPath :: String
    , _editCommentEventChangeDTOThreadUuid :: U.UUID
    , _editCommentEventChangeDTOCommentUuid :: U.UUID
    , _editCommentEventChangeDTOText :: String
    , _editCommentEventChangeDTOPrivate :: Bool
    }
  deriving (Show, Eq, Generic)

data DeleteCommentEventChangeDTO =
  DeleteCommentEventChangeDTO
    { _deleteCommentEventChangeDTOUuid :: U.UUID
    , _deleteCommentEventChangeDTOPath :: String
    , _deleteCommentEventChangeDTOThreadUuid :: U.UUID
    , _deleteCommentEventChangeDTOCommentUuid :: U.UUID
    , _deleteCommentEventChangeDTOPrivate :: Bool
    }
  deriving (Show, Eq, Generic)
