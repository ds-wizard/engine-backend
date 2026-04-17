module WizardLib.Public.Database.DAO.User.UserOpenIdIdentityDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.User.UserOpenIdIdentity ()
import WizardLib.Public.Database.Mapping.User.UserOpenIdIdentityList ()
import WizardLib.Public.Model.User.UserOpenIdIdentity
import WizardLib.Public.Model.User.UserOpenIdIdentityList

entityName = "user_openid_identity"

findUserOpenIdIdentitiesByUserUuid :: AppContextC s sc m => U.UUID -> m [UserOpenIdIdentity]
findUserOpenIdIdentitiesByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserOpenIdIdentityListsByUserUuid :: AppContextC s sc m => U.UUID -> m [UserOpenIdIdentityList]
findUserOpenIdIdentityListsByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT i.uuid, i.external_id, i.external_label, i.provider_uuid, oc.name, oc.style, i.created_at \
          \FROM user_openid_identity i \
          \JOIN openid_client oc ON oc.uuid = i.provider_uuid \
          \WHERE i.user_uuid = ? AND i.tenant_uuid = ?"
  let params = [toField userUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findUserOpenIdIdentityByExternalIdAndProvider' :: AppContextC s sc m => String -> U.UUID -> m (Maybe UserOpenIdIdentity)
findUserOpenIdIdentityByExternalIdAndProvider' externalId providerUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn'
    entityName
    [tenantQueryUuid tenantUuid, ("external_id", externalId), ("provider_uuid", U.toString providerUuid)]

insertUserOpenIdIdentity :: AppContextC s sc m => UserOpenIdIdentity -> m Int64
insertUserOpenIdIdentity = createInsertFn entityName

deleteUserOpenIdIdentities :: AppContextC s sc m => m Int64
deleteUserOpenIdIdentities = createDeleteEntitiesFn entityName

deleteUserOpenIdIdentityByUuid :: AppContextC s sc m => U.UUID -> m ()
deleteUserOpenIdIdentityByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
