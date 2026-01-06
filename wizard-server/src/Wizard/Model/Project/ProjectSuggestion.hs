module Wizard.Model.Project.ProjectSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data ProjectSuggestion = ProjectSuggestion
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  }
  deriving (Show, Eq, Generic)
