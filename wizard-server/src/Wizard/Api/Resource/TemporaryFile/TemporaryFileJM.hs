module Wizard.Api.Resource.TemporaryFile.TemporaryFileJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO

instance FromJSON TemporaryFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemporaryFileDTO where
  toJSON = genericToJSON jsonOptions
