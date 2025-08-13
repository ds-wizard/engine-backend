module Wizard.Api.Resource.TypeHint.TypeHintIJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO

instance FromJSON TypeHintLegacyIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintLegacyIDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TypeHintIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintIDTO where
  toJSON = genericToJSON jsonOptions
