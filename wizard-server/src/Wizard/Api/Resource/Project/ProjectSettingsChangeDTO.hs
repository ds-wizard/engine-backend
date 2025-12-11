module Wizard.Api.Resource.Project.ProjectSettingsChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectSettingsChangeDTO = ProjectSettingsChangeDTO
  { name :: String
  , description :: Maybe String
  , projectTags :: [String]
  , documentTemplateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  , isTemplate :: Bool
  }
  deriving (Show, Eq, Generic)
