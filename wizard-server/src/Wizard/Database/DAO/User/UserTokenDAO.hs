module Wizard.Database.DAO.User.UserTokenDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.UserToken ()
import Wizard.Database.Mapping.User.UserTokenList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserToken
import Wizard.Model.User.UserTokenList
import Wizard.Service.Cache.UserTokenCache
import Wizard.Util.Cache
import Wizard.Util.Logger

entityName = "user_token"

pageLabel = "tokens"

findUserTokens :: AppContextM [UserToken]
findUserTokens = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findUserTokensByUserUuidAndType :: U.UUID -> UserTokenType -> Maybe U.UUID -> AppContextM [UserTokenList]
findUserTokensByUserUuidAndType userUuid tokenType mCurrentTokenUuid = do
  appUuid <- asks currentAppUuid
  let currentSessionCondition =
        case mCurrentTokenUuid of
          Just _ -> "uuid = ?"
          Nothing -> "false"
  let sql =
        fromString $
          f'
            "SELECT uuid, name, user_agent, %s as current_session, expires_at, created_at \
            \FROM user_token \
            \WHERE app_uuid = ? AND user_uuid = ? AND type = ?"
            [currentSessionCondition, entityName]
  let params = (fmap U.toString . maybeToList $ mCurrentTokenUuid) ++ [U.toString appUuid, U.toString userUuid, show tokenType]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

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

findUserTokenByUuid :: U.UUID -> AppContextM UserToken
findUserTokenByUuid uuid = getFromCacheOrDb getFromCache addToCache go (U.toString uuid)
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

deleteUserTokensWithExpiration :: AppContextM Int64
deleteUserTokensWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

deleteUserTokenByUuid :: U.UUID -> AppContextM Int64
deleteUserTokenByUuid uuid = do
  appUuid <- asks currentAppUuid
  result <- createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
