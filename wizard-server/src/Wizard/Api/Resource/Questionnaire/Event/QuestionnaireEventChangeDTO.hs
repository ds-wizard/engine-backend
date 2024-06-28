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

data SetReplyEventChangeDTO = SetReplyEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  , value :: ReplyValue
  }
  deriving (Show, Eq, Generic)

data ClearReplyEventChangeDTO = ClearReplyEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  }
  deriving (Show, Eq, Generic)

data SetPhaseEventChangeDTO = SetPhaseEventChangeDTO
  { uuid :: U.UUID
  , phaseUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

data SetLabelsEventChangeDTO = SetLabelsEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  , value :: [U.UUID]
  }
  deriving (Show, Eq, Generic)

data ResolveCommentThreadEventChangeDTO = ResolveCommentThreadEventChangeDTO
  { uuid :: U.UUID
  , threadUuid :: U.UUID
  , path :: String
  , private :: Bool
  , commentCount :: Int
  }
  deriving (Show, Eq, Generic)

data ReopenCommentThreadEventChangeDTO = ReopenCommentThreadEventChangeDTO
  { uuid :: U.UUID
  , threadUuid :: U.UUID
  , path :: String
  , private :: Bool
  , commentCount :: Int
  }
  deriving (Show, Eq, Generic)

data DeleteCommentThreadEventChangeDTO = DeleteCommentThreadEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , private :: Bool
  }
  deriving (Show, Eq, Generic)

data AddCommentEventChangeDTO = AddCommentEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentUuid :: U.UUID
  , text :: String
  , private :: Bool
  , newThread :: Bool
  }
  deriving (Show, Eq, Generic)

data EditCommentEventChangeDTO = EditCommentEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentUuid :: U.UUID
  , text :: String
  , private :: Bool
  }
  deriving (Show, Eq, Generic)

data DeleteCommentEventChangeDTO = DeleteCommentEventChangeDTO
  { uuid :: U.UUID
  , path :: String
  , threadUuid :: U.UUID
  , commentUuid :: U.UUID
  , private :: Bool
  }
  deriving (Show, Eq, Generic)
