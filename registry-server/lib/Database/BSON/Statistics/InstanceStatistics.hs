module Database.BSON.Statistics.InstanceStatistics where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Model.Statistics.InstanceStatistics

instance ToBSON InstanceStatistics where
  toBSON InstanceStatistics {..} =
    [ "userCount" BSON.=: _instanceStatisticsUserCount
    , "pkgCount" BSON.=: _instanceStatisticsPkgCount
    , "qtnCount" BSON.=: _instanceStatisticsQtnCount
    ]

instance FromBSON InstanceStatistics where
  fromBSON doc = do
    _instanceStatisticsUserCount <- BSON.lookup "userCount" doc
    _instanceStatisticsPkgCount <- BSON.lookup "pkgCount" doc
    _instanceStatisticsQtnCount <- BSON.lookup "qtnCount" doc
    return InstanceStatistics {..}
