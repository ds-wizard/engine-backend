module Wizard.Model.Project.ProjectSimple where

import qualified Data.UUID as U
import GHC.Generics

data ProjectSimple = ProjectSimple
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Generic, Eq, Show)
