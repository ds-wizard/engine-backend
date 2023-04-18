module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO

instance FromJSON QuestionnaireImporterChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireImporterChangeDTO where
  toJSON = genericToJSON jsonOptions
