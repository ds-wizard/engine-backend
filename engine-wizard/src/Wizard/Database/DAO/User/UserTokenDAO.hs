module Wizard.Database.DAO.User.UserTokenDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Util.String (f')
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.UserToken ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserToken
import Wizard.Service.Cache.UserTokenCache
import Wizard.Util.Cache

entityName = "user_token"

pageLabel = "tokens"

findUserTokens :: AppContextM [UserToken]
findUserTokens = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findUserTokensByUserUuid :: U.UUID -> AppContextM [UserToken]
findUserTokensByUserUuid userUuid = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("user_uuid", U.toString userUuid)]

findUserTokensBySessionState :: String -> AppContextM [UserToken]
findUserTokensBySessionState sessionState = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("session_state", sessionState)]

findUserTokensByValue :: String -> AppContextM [UserToken]
findUserTokensByValue value = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("value", value)]

findUserTokenById :: String -> AppContextM UserToken
findUserTokenById = getFromCacheOrDb getFromCache addToCache go
  where
    go uuid = do
      appUuid <- asks currentAppUuid
      createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertUserToken :: UserToken -> AppContextM Int64
insertUserToken userToken = do
  result <- createInsertFn entityName userToken
  addToCache userToken
  return result

deleteUserTokens :: AppContextM Int64
deleteUserTokens = createDeleteEntitiesFn entityName

deleteUserTokensWithExpiration :: Integer -> AppContextM Int64
deleteUserTokensWithExpiration expirationInHours = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at <= now() - interval '? hour'" [entityName]
  let params = [toField expirationInHours]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserTokenById :: U.UUID -> AppContextM Int64
deleteUserTokenById uuid = do
  appUuid <- asks currentAppUuid
  result <- createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
