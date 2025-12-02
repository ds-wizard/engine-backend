module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateCreateDTO where
  toJSON = genericToJSON jsonOptions
