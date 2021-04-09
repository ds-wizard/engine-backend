module Registry.Database.DAO.Audit.AuditEntrySqlDAO where

import GHC.Int

import Registry.Database.Mapping.Audit.AuditEntry ()
import Registry.Model.Audit.AuditEntry
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.CommonSql

entityName = "audit"

findAuditEntries :: AppContextM [AuditEntry]
findAuditEntries = createFindEntitiesFn entityName

insertAuditEntry :: AuditEntry -> AppContextM Int64
insertAuditEntry = createInsertFn entityName

deleteAuditEntries :: AppContextM Int64
deleteAuditEntries = createDeleteEntitiesFn entityName
