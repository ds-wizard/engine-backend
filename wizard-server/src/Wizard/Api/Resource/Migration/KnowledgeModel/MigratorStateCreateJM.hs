module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateCreateDTO where
  toJSON = genericToJSON jsonOptions
