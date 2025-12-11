module Wizard.Model.Project.File.ProjectFileList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Model.Project.ProjectSimple
import WizardLib.Public.Model.User.UserSuggestion

data ProjectFileList = ProjectFileList
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , project :: ProjectSimple
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
