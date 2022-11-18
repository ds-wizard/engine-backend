module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON QuestionnaireImporterDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireImporterDTO where
  toJSON = genericToJSON jsonOptions
