module Registry.Database.BSON.Audit.AuditEntry where

import Data.Bson.Generic

import Registry.Database.BSON.Statistics.InstanceStatistics ()
import Registry.Model.Audit.AuditEntry

instance ToBSON AuditEntry

instance FromBSON AuditEntry
