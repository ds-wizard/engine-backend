module Wizard.Database.Migration.Development.Audit.AuditMigration where

import Shared.Common.Constant.Component
import Wizard.Database.DAO.Audit.AuditDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Audit/Audit) started"
  deleteAudits
  logInfo _CMP_MIGRATION "(Audit/Audit) ended"
