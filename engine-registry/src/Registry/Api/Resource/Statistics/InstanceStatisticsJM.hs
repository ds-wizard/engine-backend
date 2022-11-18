module Registry.Api.Resource.Statistics.InstanceStatisticsJM where

import Data.Aeson

import Registry.Model.Statistics.InstanceStatistics
import Shared.Util.Aeson

instance FromJSON InstanceStatistics where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON InstanceStatistics where
  toJSON = genericToJSON jsonOptions
