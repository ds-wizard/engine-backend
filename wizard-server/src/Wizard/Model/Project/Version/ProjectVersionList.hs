module Wizard.Model.Project.Version.ProjectVersionList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.UserSuggestion

data ProjectVersionList = ProjectVersionList
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , eventUuid :: U.UUID
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ProjectVersionList where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.eventUuid == b.eventUuid
      && a.createdBy == b.createdBy
