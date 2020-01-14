module Registry.Database.Migration.Development.Audit.AuditMigration where

import Registry.Constant.Component
import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Audit/Audit) started"
  deleteAuditEntries
  logInfo $ msg _CMP_MIGRATION "(Audit/Audit) ended"
