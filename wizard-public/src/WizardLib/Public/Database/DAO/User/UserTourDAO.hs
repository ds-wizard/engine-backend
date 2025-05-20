module WizardLib.Public.Database.DAO.User.UserTourDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.User.UserTour ()
import WizardLib.Public.Model.User.UserTour

entityName = "user_tour"

findUserToursByUserUuid :: AppContextC s sc m => U.UUID -> m [String]
findUserToursByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        "SELECT tour_id \
        \FROM user_tour \
        \WHERE tenant_uuid = ? AND user_uuid = ?"
  let params = [U.toString tenantUuid, U.toString userUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  return . concat $ entities

insertUserTour :: AppContextC s sc m => UserTour -> m Int64
insertUserTour = createInsertFn entityName

deleteTours :: AppContextC s sc m => m Int64
deleteTours = createDeleteEntitiesFn entityName

deleteToursByUserUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteToursByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]
