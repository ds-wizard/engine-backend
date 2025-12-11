module Wizard.Model.Project.File.ProjectFileSimple where

import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data ProjectFileSimple = ProjectFileSimple
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  }
  deriving (Show, Eq, Generic)
