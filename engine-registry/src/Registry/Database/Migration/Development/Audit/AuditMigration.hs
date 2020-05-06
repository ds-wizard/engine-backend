module Registry.Database.Migration.Development.Audit.AuditMigration where

import Registry.Constant.Component
import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Audit/Audit) started"
  deleteAuditEntries
  logInfo _CMP_MIGRATION "(Audit/Audit) ended"
