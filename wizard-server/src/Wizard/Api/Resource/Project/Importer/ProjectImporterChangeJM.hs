module Wizard.Api.Resource.Project.Importer.ProjectImporterChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeDTO

instance FromJSON ProjectImporterChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectImporterChangeDTO where
  toJSON = genericToJSON jsonOptions
