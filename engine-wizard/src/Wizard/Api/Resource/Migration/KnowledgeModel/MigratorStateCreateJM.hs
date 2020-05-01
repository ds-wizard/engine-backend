module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MigratorStateCreateDTO where
  toJSON = genericToJSON simpleOptions
