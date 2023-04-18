module Registry.Database.DAO.Audit.AuditEntryDAO where

import GHC.Int

import Registry.Database.DAO.Common
import Registry.Database.Mapping.Audit.AuditEntry ()
import Registry.Model.Audit.AuditEntry
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()

entityName = "audit"

findAuditEntries :: AppContextM [AuditEntry]
findAuditEntries = createFindEntitiesFn entityName

insertAuditEntry :: AuditEntry -> AppContextM Int64
insertAuditEntry = createInsertFn entityName

deleteAuditEntries :: AppContextM Int64
deleteAuditEntries = createDeleteEntitiesFn entityName
