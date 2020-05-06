module Registry.Database.BSON.Statistics.InstanceStatistics where

import Data.Bson.Generic

import Registry.Model.Statistics.InstanceStatistics

instance ToBSON InstanceStatistics

instance FromBSON InstanceStatistics
