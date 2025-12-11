module Wizard.Service.Project.Importer.ProjectImporterMapper where

import Data.Time

import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Model.Project.Importer.ProjectImporter

toDTO :: ProjectImporter -> ProjectImporterDTO
toDTO importer =
  ProjectImporterDTO
    { piId = importer.piId
    , name = importer.name
    , description = importer.description
    , url = importer.url
    , enabled = importer.enabled
    , createdAt = importer.createdAt
    , updatedAt = importer.updatedAt
    }

toChangeDTO :: ProjectImporter -> ProjectImporterChangeDTO
toChangeDTO importer = ProjectImporterChangeDTO {enabled = importer.enabled}

fromChangeDTO :: ProjectImporter -> ProjectImporterChangeDTO -> UTCTime -> ProjectImporter
fromChangeDTO importer reqDto now =
  importer {enabled = reqDto.enabled, updatedAt = now}
