module Wizard.Api.Resource.Project.ProjectSettingsChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Api.Resource.Project.ProjectSettingsChangeJM ()
import Wizard.Database.Migration.Development.Project.Data.Projects

instance ToSchema ProjectSettingsChangeDTO where
  declareNamedSchema = toSwagger project1SettingsChange
