module Wizard.Api.Resource.Project.Importer.ProjectImporterChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import Wizard.Service.Project.Importer.ProjectImporterMapper

instance ToSchema ProjectImporterChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO projectImporterBio1)
