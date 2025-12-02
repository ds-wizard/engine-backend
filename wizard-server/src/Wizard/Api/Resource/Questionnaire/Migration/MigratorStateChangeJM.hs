module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO

instance FromJSON MigratorStateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateChangeDTO where
  toJSON = genericToJSON jsonOptions
