module Registry.Api.Resource.Statistics.InstanceStatisticsJM where

import Data.Aeson

import Registry.Model.Statistics.InstanceStatistics
import Shared.Util.JSON

instance FromJSON InstanceStatistics where
  parseJSON = simpleParseJSON "_instanceStatistics"

instance ToJSON InstanceStatistics where
  toJSON = simpleToJSON "_instanceStatistics"
