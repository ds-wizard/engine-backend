module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM ()
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper

instance ToSchema QuestionnaireImporterDTO where
  declareNamedSchema = toSwagger (toDTO questionnaireImporterBio1)
