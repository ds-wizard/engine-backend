module WizardLib.Public.Database.DAO.User.UserTokenDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Cache as C
import Data.Maybe (maybeToList)
import Data.Pool
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
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

findUserTokens
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m [UserToken]
findUserTokens = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findUserTokensByUserUuidAndType
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> UserTokenType
  -> Maybe U.UUID
  -> m [UserTokenList]
findUserTokensByUserUuidAndType userUuid tokenType mCurrentTokenUuid = do
  appUuid <- asks (.appUuid')
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

findUserTokensByUserUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m [UserToken]
findUserTokensByUserUuid userUuid = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("user_uuid", U.toString userUuid)]

findUserTokensBySessionState
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m [UserToken]
findUserTokensBySessionState sessionState = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("session_state", sessionState)]

findUserTokensByValue
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m [UserToken]
findUserTokensByValue value = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("value", value)]

findUserTokenByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , MonadIO m
     )
  => U.UUID
  -> m UserToken
findUserTokenByUuid uuid = getFromCacheOrDb getFromCache addToCache go (U.toString uuid)
  where
    go uuid = do
      appUuid <- asks (.appUuid')
      createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertUserToken
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , MonadIO m
     )
  => UserToken
  -> m Int64
insertUserToken userToken = do
  result <- createInsertFn entityName userToken
  addToCache userToken
  return result

deleteUserTokens
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
deleteUserTokens = createDeleteEntitiesFn entityName

deleteUserTokensWithExpiration
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
deleteUserTokensWithExpiration = do
  let sql = f' "DELETE FROM %s WHERE expires_at <= now()" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

deleteUserTokenByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , MonadIO m
     )
  => U.UUID
  -> m Int64
deleteUserTokenByUuid uuid = do
  appUuid <- asks (.appUuid')
  result <- createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
  deleteFromCache (U.toString uuid)
  return result
