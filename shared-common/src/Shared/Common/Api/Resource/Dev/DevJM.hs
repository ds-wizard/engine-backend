module Shared.Common.Api.Resource.Dev.DevJM where

import Data.Aeson

import Shared.Common.Model.Dev.Dev
import Shared.Common.Util.Aeson

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