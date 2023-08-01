module Shared.Audit.Database.Migration.Development.Audit.AuditMigration where

import Shared.Audit.Database.DAO.Audit.AuditDAO
import Shared.Common.Constant.Component
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Audit/Audit) started"
  deleteAudits
  logInfo _CMP_MIGRATION "(Audit/Audit) ended"
