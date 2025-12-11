module Wizard.Model.Project.File.ProjectFile where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data ProjectFile = ProjectFile
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , projectUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
