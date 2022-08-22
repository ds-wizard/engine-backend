module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO

instance FromJSON QuestionnaireImporterChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireImporterChangeDTO where
  toJSON = genericToJSON simpleOptions
