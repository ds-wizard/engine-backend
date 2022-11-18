module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateCreateDTO where
  toJSON = genericToJSON jsonOptions
