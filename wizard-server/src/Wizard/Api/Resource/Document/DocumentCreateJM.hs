module Wizard.Api.Resource.Document.DocumentCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Document.DocumentCreateDTO

instance FromJSON DocumentCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentCreateDTO where
  toJSON = genericToJSON jsonOptions
