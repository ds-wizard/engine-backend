module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper where

import Control.Lens ((^.))
import Data.Time

import LensesConfig
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

toDTO :: QuestionnaireImporter -> QuestionnaireImporterDTO
toDTO importer =
  QuestionnaireImporterDTO
    { _questionnaireImporterDTOQiId = importer ^. qiId
    , _questionnaireImporterDTOName = importer ^. name
    , _questionnaireImporterDTODescription = importer ^. description
    , _questionnaireImporterDTOUrl = importer ^. url
    , _questionnaireImporterDTOEnabled = importer ^. enabled
    , _questionnaireImporterDTOCreatedAt = importer ^. createdAt
    , _questionnaireImporterDTOUpdatedAt = importer ^. updatedAt
    }

toChangeDTO :: QuestionnaireImporter -> QuestionnaireImporterChangeDTO
toChangeDTO importer = QuestionnaireImporterChangeDTO {_questionnaireImporterChangeDTOEnabled = importer ^. enabled}

fromChangeDTO :: QuestionnaireImporter -> QuestionnaireImporterChangeDTO -> UTCTime -> QuestionnaireImporter
fromChangeDTO importer reqDto now =
  importer {_questionnaireImporterEnabled = reqDto ^. enabled, _questionnaireImporterUpdatedAt = now}
