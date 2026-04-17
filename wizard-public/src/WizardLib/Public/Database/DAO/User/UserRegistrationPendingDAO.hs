module WizardLib.Public.Database.DAO.User.UserRegistrationPendingDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.User.UserRegistrationPending ()
import WizardLib.Public.Model.User.UserRegistrationPending

entityName = "user_registration_pending"

findUserRegistrationPendingByHash
  :: (AppContextC s sc m, FromField serviceType) => String -> m (UserRegistrationPending serviceType)
findUserRegistrationPendingByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]

findUserRegistrationPendingByServiceTypeAndExternalIdAndProviderUuid'
  :: (AppContextC s sc m, FromField serviceType, Show serviceType)
  => serviceType
  -> String
  -> U.UUID
  -> m (Maybe (UserRegistrationPending serviceType))
findUserRegistrationPendingByServiceTypeAndExternalIdAndProviderUuid' serviceType externalId providerUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn'
    entityName
    [ tenantQueryUuid tenantUuid
    , ("service_type", show serviceType)
    , ("external_id", externalId)
    , ("provider_uuid", U.toString providerUuid)
    ]

insertUserRegistrationPending
  :: (AppContextC s sc m, ToField serviceType) => UserRegistrationPending serviceType -> m Int64
insertUserRegistrationPending = createInsertFn entityName

updateUserRegistrationPendingByUuid
  :: (AppContextC s sc m, ToField serviceType) => UserRegistrationPending serviceType -> m Int64
updateUserRegistrationPendingByUuid pending = do
  let sql =
        fromString
          "UPDATE user_registration_pending SET uuid = ?, hash = ?, service_type = ?, provider_uuid = ?, external_id = ?, external_label = ?, email = ?, first_name = ?, last_name = ?, image_url = ?, affiliation = ?, tenant_uuid = ?, created_at = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow pending ++ [toField pending.uuid, toField pending.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserRegistrationPendings :: AppContextC s sc m => m Int64
deleteUserRegistrationPendings = createDeleteEntitiesFn entityName

deleteUserRegistrationPendingByUuid :: AppContextC s sc m => U.UUID -> m ()
deleteUserRegistrationPendingByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()

deleteUserRegistrationPendingByHash :: AppContextC s sc m => String -> m ()
deleteUserRegistrationPendingByHash hash = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("hash", hash)]
  return ()

deleteUserRegistrationPendingsOlderThan :: AppContextC s sc m => UTCTime -> m Int64
deleteUserRegistrationPendingsOlderThan threshold = do
  let sql = fromString "DELETE FROM user_registration_pending WHERE created_at < ?"
  let params = [toField threshold]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
