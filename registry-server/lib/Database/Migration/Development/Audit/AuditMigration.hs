module Database.Migration.Development.Audit.AuditMigration where

import Constant.Component
import Database.DAO.Audit.AuditEntryDAO
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Audit/Audit) started"
  deleteAuditEntries
  logInfo $ msg _CMP_MIGRATION "(Audit/Audit) ended"
