module Wizard.Api.Resource.Usage.UsageJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Usage.UsageDTO

instance FromJSON UsageDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UsageDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON UsageEntryDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UsageEntryDTO where
  toJSON = genericToJSON simpleOptions
