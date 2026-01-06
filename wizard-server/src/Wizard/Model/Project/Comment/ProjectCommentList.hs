module Wizard.Model.Project.Comment.ProjectCommentList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.UserSuggestion

data ProjectCommentThreadList = ProjectCommentThreadList
  { uuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , comments :: [ProjectCommentList]
  , private :: Bool
  , assignedTo :: Maybe UserSuggestion
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data ProjectCommentList = ProjectCommentList
  { uuid :: U.UUID
  , text :: String
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord ProjectCommentList where
  compare a b = compare a.uuid b.uuid
