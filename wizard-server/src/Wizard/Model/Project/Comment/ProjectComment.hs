module Wizard.Model.Project.Comment.ProjectComment where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ProjectCommentThread = ProjectCommentThread
  { uuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , comments :: [ProjectComment]
  , private :: Bool
  , projectUuid :: U.UUID
  , assignedTo :: Maybe U.UUID
  , assignedBy :: Maybe U.UUID
  , notificationRequired :: Bool
  , createdBy :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data ProjectComment = ProjectComment
  { uuid :: U.UUID
  , text :: String
  , threadUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
