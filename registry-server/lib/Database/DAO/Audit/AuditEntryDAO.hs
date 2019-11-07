module Database.DAO.Audit.AuditEntryDAO where

import Data.Bson

import Database.BSON.Audit.AuditEntry ()
import Database.DAO.Common
import Model.Audit.AuditEntry
import Model.Context.AppContext
import Model.Error.Error

entityName = "audit"

collection = "auditEntries"

findAuditEntries :: AppContextM (Either AppError [AuditEntry])
findAuditEntries = createFindEntitiesFn collection

insertAuditEntry :: AuditEntry -> AppContextM Value
insertAuditEntry = createInsertFn collection

deleteAuditEntries :: AppContextM ()
deleteAuditEntries = createDeleteEntitiesFn collection
