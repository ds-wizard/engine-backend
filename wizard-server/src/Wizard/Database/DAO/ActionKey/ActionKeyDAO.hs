module Wizard.Database.DAO.ActionKey.ActionKeyDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.ActionKey.ActionKey ()
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "action_key"

findActionKeys :: AppContextM [ActionKey]
findActionKeys = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findActionKeyByHash :: String -> AppContextM ActionKey
findActionKeyByHash hash = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

findActionKeyByUserIdAndHash' :: U.UUID -> String -> AppContextM (Maybe ActionKey)
findActionKeyByUserIdAndHash' userId hash = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("user_id", U.toString userId), ("hash", hash)]

insertActionKey :: ActionKey -> AppContextM Int64
insertActionKey = createInsertFn entityName

deleteActionKeys :: AppContextM Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash :: String -> AppContextM Int64
deleteActionKeyByHash hash = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

deleteActionKeyByUserId :: U.UUID -> AppContextM Int64
deleteActionKeyByUserId userId = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("user_id", U.toString userId)]

deleteActionKeyByUserIdAndHash :: U.UUID -> String -> AppContextM Int64
deleteActionKeyByUserIdAndHash userId hash = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("user_id", U.toString userId), ("hash", hash)]

deleteActionKeyOlderThen :: UTCTime -> AppContextM Int64
deleteActionKeyOlderThen date = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
