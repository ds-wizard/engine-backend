module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO

instance FromJSON MigratorStateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateChangeDTO where
  toJSON = genericToJSON jsonOptions
