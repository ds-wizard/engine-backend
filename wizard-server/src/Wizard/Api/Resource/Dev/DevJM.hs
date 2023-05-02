module Wizard.Api.Resource.Dev.DevJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Dev.Dev

instance FromJSON DevSection where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevSection where
  toJSON = genericToJSON jsonOptions

instance FromJSON DevOperation where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevOperation where
  toJSON = genericToJSON jsonOptions

instance FromJSON DevOperationParameter where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevOperationParameter where
  toJSON = genericToJSON jsonOptions

instance FromJSON DevOperationParameterType

instance ToJSON DevOperationParameterType
