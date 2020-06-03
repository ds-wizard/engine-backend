module Registry.Database.DAO.Audit.AuditEntryDAO where

import Data.Bson

import Registry.Database.BSON.Audit.AuditEntry ()
import Registry.Model.Audit.AuditEntry
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextLenses ()
import Shared.Database.DAO.Common

entityName = "audit"

collection = "auditEntries"

findAuditEntries :: AppContextM [AuditEntry]
findAuditEntries = createFindEntitiesFn collection

insertAuditEntry :: AuditEntry -> AppContextM Value
insertAuditEntry = createInsertFn collection

deleteAuditEntries :: AppContextM ()
deleteAuditEntries = createDeleteEntitiesFn collection
