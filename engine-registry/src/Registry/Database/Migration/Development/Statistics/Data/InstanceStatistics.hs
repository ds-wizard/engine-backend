module Registry.Database.Migration.Development.Statistics.Data.InstanceStatistics where

import Registry.Model.Statistics.InstanceStatistics

iStat :: InstanceStatistics
iStat =
  InstanceStatistics
    { userCount = 10
    , pkgCount = 20
    , qtnCount = 30
    , branchCount = 40
    , docCount = 50
    , tmlCount = 60
    }
