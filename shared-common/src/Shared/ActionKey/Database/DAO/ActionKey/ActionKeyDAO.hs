module Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.ActionKey.Database.Mapping.ActionKey.ActionKey ()
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

entityName = "action_key"

findActionKeys :: (AppContextC s sc m, FromField aType, FromField identity) => m [ActionKey identity aType]
findActionKeys = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findActionKeyByHash :: (AppContextC s sc m, FromField aType, FromField identity) => String -> m (ActionKey identity aType)
findActionKeyByHash hash = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

findActionKeyByIdentityAndHash'
  :: ( AppContextC s sc m
     , FromField aType
     , FromField identity
     , ToField identity
     )
  => String
  -> String
  -> m (Maybe (ActionKey identity aType))
findActionKeyByIdentityAndHash' identity hash = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("identity", identity), ("hash", hash)]

insertActionKey :: (AppContextC s sc m, ToField aType, ToField identity) => ActionKey identity aType -> m Int64
insertActionKey = createInsertFn entityName

deleteActionKeys :: AppContextC s sc m => m Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash :: AppContextC s sc m => String -> m Int64
deleteActionKeyByHash hash = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

deleteActionKeyByIdentity :: AppContextC s sc m => String -> m Int64
deleteActionKeyByIdentity identity = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("identity", identity)]

deleteActionKeyByIdentityAndHash :: AppContextC s sc m => String -> String -> m Int64
deleteActionKeyByIdentityAndHash identity hash = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("identity", identity), ("hash", hash)]

deleteActionKeyOlderThen :: AppContextC s sc m => UTCTime -> m Int64
deleteActionKeyOlderThen date = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
