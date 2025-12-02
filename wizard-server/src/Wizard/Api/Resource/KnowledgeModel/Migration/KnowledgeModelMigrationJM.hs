module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()

instance FromJSON KnowledgeModelMigrationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelMigrationDTO where
  toJSON = genericToJSON jsonOptions
