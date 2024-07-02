module WizardLib.Public.Database.DAO.User.UserTokenDAO where

import Control.Monad.Reader (asks)
import qualified Data.Cache as C
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Cache
import Shared.Common.Util.Logger
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.User.UserToken ()
import WizardLib.Public.Database.Mapping.User.UserTokenList ()
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Model.User.UserTokenList
import WizardLib.Public.Service.Cache.UserTokenCache

entityName = "user_token"

pageLabel = "tokens"

findUserTokens :: AppContextC s sc m => m [UserToken]
findUserTokens = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findUserTokensByUserUuidAndTypeAndTokenUuid :: AppContextC s sc m => U.UUID -> UserTokenType -> Maybe U.UUID -> m [UserTokenList]
findUserTokensByUserUuidAndTypeAndTokenUuid userUuid tokenType mCurrentTokenUuid = do
  tenantUuid <- asks (.tenantUuid')
  let currentSessionCondition =
        case mCurrentTokenUuid of
          Just _ -> "uuid = ?"
          Nothing -> "false"
  let sql =
        fromString $
          f'
            "SELECT uuid, name, user_agent, %s as current_session, expires_at, created_at \
            \FROM %s \
            \WHERE tenant_uuid = ? AND user_uuid = ? AND type = ?"
            [currentSessionCondition, entityName]
  let params = (fmap U.toString . maybeToList $ mCurrentTokenUuid) ++ [U.toString tenantUuid, U.toString userUuid, show tokenType]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findApiUserTokensWithCloseExpiration :: AppContextC s sc m => m [UserToken]
findApiUserTokensWithCloseExpiration = do
  let sql =
        f'
          "SELECT * \
          \FROM %s \
          \WHERE type = '%s' AND ((now() + interval '1 day' < expires_at AND expires_at < now() + interval '2 day') OR (now() + interval '6 day' < expires_at AND expires_at < now() + interval '7 day'))"
          [entityName, show ApiKeyUserTokenType]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findUserTokensByUserUuid :: AppContextC s sc m => U.UUID -> m [UserToken]
findUserTokensByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserTokensByUserUuidAndType :: AppContextC s sc m => U.UUID -> String -> m [UserToken]
findUserTokensByUserUuidAndType userUuid aType = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid), ("type", aType)]

findUserTokensBySessionState :: AppContextC s sc m => String -> m [UserToken]
findUserTokensBySessionState sessionState = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("session_state", sessionState)]

findUserTokensByValue :: AppContextC s sc m => String -> m [UserToken]
findUserTokensByValue value = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("value", value)]

findUserTokensByTenantUuid :: AppContextC s sc m => U.UUID -> m [UserToken]
findUserTokensByTenantUuid tenantUuid = do
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findUserTokenByUuid
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => U.UUID
  -> m UserToken
findUserTokenByUuid uuid = getFromCacheOrDb getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      tenantUuid <- asks (.tenantUuid')
      createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", uuid)]

insertUserToken
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => UserToken
  -> m Int64
insertUserToken userToken = do
  result <- createInsertFn entityName userToken
  addToCache userToken
  return result

deleteUserTokens :: AppContextC s sc m => m Int64
deleteUserTokens = createDeleteEntitiesFn entityName

deleteUserTokensWithExpiration :: AppContextC s sc m => m Int64
deleteUserTokensWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

deleteUserTokenByUuid
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => U.UUID
  -> m Int64
deleteUserTokenByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  result <- createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result

deleteUserTokenByUuidAndTenantUuid
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => U.UUID
  -> U.UUID
  -> m Int64
deleteUserTokenByUuidAndTenantUuid uuid tenantUuid = do
  result <- createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
