module Wizard.Database.DAO.Audit.AuditDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Audit.Audit ()
import Wizard.Model.Audit.Audit
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "audit"

findAudits :: AppContextM [Audit]
findAudits = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

insertAudit :: Audit -> AppContextM Int64
insertAudit = createInsertWithoutTransactionFn entityName

deleteAudits :: AppContextM Int64
deleteAudits = createDeleteEntitiesFn entityName
