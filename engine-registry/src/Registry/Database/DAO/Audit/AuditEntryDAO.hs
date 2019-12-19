module Registry.Database.DAO.Audit.AuditEntryDAO where

import Data.Bson

import Registry.Database.BSON.Audit.AuditEntry ()
import Registry.Database.DAO.Common
import Registry.Model.Audit.AuditEntry
import Registry.Model.Context.AppContext
import Shared.Model.Error.Error

entityName = "audit"

collection = "auditEntries"

findAuditEntries :: AppContextM (Either AppError [AuditEntry])
findAuditEntries = createFindEntitiesFn collection

insertAuditEntry :: AuditEntry -> AppContextM Value
insertAuditEntry = createInsertFn collection

deleteAuditEntries :: AppContextM ()
deleteAuditEntries = createDeleteEntitiesFn collection
