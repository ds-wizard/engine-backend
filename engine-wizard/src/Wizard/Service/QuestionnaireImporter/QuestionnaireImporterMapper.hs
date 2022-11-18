module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper where

import Data.Time

import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

toDTO :: QuestionnaireImporter -> QuestionnaireImporterDTO
toDTO importer =
  QuestionnaireImporterDTO
    { qiId = importer.qiId
    , name = importer.name
    , description = importer.description
    , url = importer.url
    , enabled = importer.enabled
    , createdAt = importer.createdAt
    , updatedAt = importer.updatedAt
    }

toChangeDTO :: QuestionnaireImporter -> QuestionnaireImporterChangeDTO
toChangeDTO importer = QuestionnaireImporterChangeDTO {enabled = importer.enabled}

fromChangeDTO :: QuestionnaireImporter -> QuestionnaireImporterChangeDTO -> UTCTime -> QuestionnaireImporter
fromChangeDTO importer reqDto now =
  importer {enabled = reqDto.enabled, updatedAt = now}
