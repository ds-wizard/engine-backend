module Database.Migration.Development.Statistics.Data.InstanceStatistics where

import Model.Statistics.InstanceStatistics

iStat :: InstanceStatistics
iStat =
  InstanceStatistics
  {_instanceStatisticsUserCount = 10, _instanceStatisticsPkgCount = 20, _instanceStatisticsQtnCount = 30}
