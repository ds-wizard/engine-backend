module Shared.Audit.Database.DAO.Audit.AuditDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Audit.Database.Mapping.Audit.Audit ()
import Shared.Audit.Model.Audit.Audit
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext

entityName = "audit"

findAudits :: AppContextC s sc m => m [Audit]
findAudits = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

insertAudit :: AppContextC s sc m => Audit -> m Int64
insertAudit = createInsertWithoutTransactionFn entityName

deleteAudits :: AppContextC s sc m => m Int64
deleteAudits = createDeleteEntitiesFn entityName

deleteAuditByCreatedBy :: AppContextC s sc m => U.UUID -> m Int64
deleteAuditByCreatedBy createdBy = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("created_by", U.toString createdBy)]
