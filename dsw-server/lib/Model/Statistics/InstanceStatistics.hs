module Model.Statistics.InstanceStatistics where

import GHC.Generics

data InstanceStatistics = InstanceStatistics
  { _instanceStatisticsUserCount :: Int
  , _instanceStatisticsPkgCount :: Int
  , _instanceStatisticsQtnCount :: Int
  } deriving (Show, Eq, Generic)
