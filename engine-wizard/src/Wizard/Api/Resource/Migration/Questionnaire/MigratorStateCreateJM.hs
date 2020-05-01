module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MigratorStateCreateDTO where
  toJSON = genericToJSON simpleOptions
