module Wizard.Model.Statistics.InstanceStatistics where

import GHC.Generics

data InstanceStatistics =
  InstanceStatistics
    { _instanceStatisticsUserCount :: Int
    , _instanceStatisticsPkgCount :: Int
    , _instanceStatisticsQtnCount :: Int
    , _instanceStatisticsBranchCount :: Int
    , _instanceStatisticsDocCount :: Int
    , _instanceStatisticsTmlCount :: Int
    }
  deriving (Show, Eq, Generic)
