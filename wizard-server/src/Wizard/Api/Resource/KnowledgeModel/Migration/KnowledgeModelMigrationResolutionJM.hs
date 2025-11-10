module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationActionJM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()

instance FromJSON KnowledgeModelMigrationResolutionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelMigrationResolutionDTO where
  toJSON = genericToJSON jsonOptions
