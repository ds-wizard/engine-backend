module Wizard.Api.Resource.Usage.UsageJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Usage.UsageDTO

instance FromJSON UsageDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UsageDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON UsageEntryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UsageEntryDTO where
  toJSON = genericToJSON jsonOptions
