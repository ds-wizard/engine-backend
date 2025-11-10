module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO

instance FromJSON KnowledgeModelMigrationCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelMigrationCreateDTO where
  toJSON = genericToJSON jsonOptions
