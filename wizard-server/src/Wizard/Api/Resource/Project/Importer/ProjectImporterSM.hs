module Wizard.Api.Resource.Project.Importer.ProjectImporterSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import Wizard.Service.Project.Importer.ProjectImporterMapper

instance ToSchema ProjectImporterDTO where
  declareNamedSchema = toSwagger (toDTO projectImporterBio1)
