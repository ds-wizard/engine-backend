module Registry.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Registry.Api.Resource.Config.ClientConfigDTO
import Shared.Common.Util.Aeson

instance FromJSON ClientConfigDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAuthDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAuthDTO where
  toJSON = genericToJSON jsonOptions
