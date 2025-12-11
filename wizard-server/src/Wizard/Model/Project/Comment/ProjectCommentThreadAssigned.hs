module Wizard.Model.Project.Comment.ProjectCommentThreadAssigned where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.UserSuggestion

data ProjectCommentThreadAssigned = ProjectCommentThreadAssigned
  { projectUuid :: U.UUID
  , projectName :: String
  , commentThreadUuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , private :: Bool
  , text :: String
  , createdBy :: Maybe UserSuggestion
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
