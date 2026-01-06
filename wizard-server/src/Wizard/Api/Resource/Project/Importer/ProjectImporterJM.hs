module Wizard.Api.Resource.Project.Importer.ProjectImporterJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON ProjectImporterDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectImporterDTO where
  toJSON = genericToJSON jsonOptions
