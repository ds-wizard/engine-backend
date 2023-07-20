module Shared.Audit.Database.DAO.Audit.AuditDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Shared.Audit.Database.Mapping.Audit.Audit ()
import Shared.Audit.Model.Audit.Audit
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext

entityName = "audit"

findAudits :: AppContextC s sc m => m [Audit]
findAudits = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

insertAudit :: AppContextC s sc m => Audit -> m Int64
insertAudit = createInsertWithoutTransactionFn entityName

deleteAudits :: AppContextC s sc m => m Int64
deleteAudits = createDeleteEntitiesFn entityName
