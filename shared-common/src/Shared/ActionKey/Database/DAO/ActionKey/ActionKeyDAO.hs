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

findActionKeys :: (AppContextC s sc m, FromField identity, FromField aType) => m [ActionKey identity aType]
findActionKeys = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findActionKeyByHash :: (AppContextC s sc m, FromField identity, FromField aType) => String -> m (ActionKey identity aType)
findActionKeyByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]

findActionKeyByHashAndType :: (AppContextC s sc m, FromField identity, FromField aType, Show aType) => String -> aType -> m (ActionKey identity aType)
findActionKeyByHashAndType hash aType = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash), ("type", show aType)]

findActionKeyByIdentityAndType' :: (AppContextC s sc m, ToField identity, FromField identity, FromField aType, Show aType) => String -> aType -> m (Maybe (ActionKey identity aType))
findActionKeyByIdentityAndType' identity aType = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("identity", identity), ("type", show aType)]

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
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("identity", identity), ("hash", hash)]

insertActionKey :: (AppContextC s sc m, ToField aType, ToField identity) => ActionKey identity aType -> m Int64
insertActionKey = createInsertFn entityName

deleteActionKeys :: AppContextC s sc m => m Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash :: AppContextC s sc m => String -> m Int64
deleteActionKeyByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]

deleteActionKeyByIdentity :: AppContextC s sc m => String -> m Int64
deleteActionKeyByIdentity identity = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("identity", identity)]

deleteActionKeyByIdentityAndHash :: AppContextC s sc m => String -> String -> m Int64
deleteActionKeyByIdentityAndHash identity hash = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("identity", identity), ("hash", hash)]

deleteActionKeyOlderThen :: AppContextC s sc m => UTCTime -> m Int64
deleteActionKeyOlderThen date = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
