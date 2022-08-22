module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON QuestionnaireImporterDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireImporterDTO where
  toJSON = genericToJSON simpleOptions
