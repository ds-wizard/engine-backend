module WizardLib.Public.Service.UserToken.UserTokenService where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.Cache as C
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Model.User.UserTokenList

getTokens
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
  => UserTokenType
  -> Maybe U.UUID
  -> m [UserTokenList]
getTokens tokenType mCurrentTokenUuid = do
  context <- ask
  let mIdentityUuid = fmap (fromJust . U.fromString) context.identity'
  case mIdentityUuid of
    Just identityUuid -> findUserTokensByUserUuidAndType identityUuid tokenType mCurrentTokenUuid
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

deleteTokensExceptCurrentSession
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
  => String
  -> m ()
deleteTokensExceptCurrentSession tokenValue =
  runInTransaction logInfoI logWarnI $ do
    context <- ask
    let mIdentityUuid = fmap (fromJust . U.fromString) context.identity'
    case mIdentityUuid of
      Just identityUuid -> do
        userTokens <- findUserTokensByUserUuid identityUuid
        traverse_ deleteUserTokenByUuid . fmap (.uuid) . filter (\ut -> ut.value /= tokenValue) $ userTokens
      Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

deleteTokenByUuid
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
  -> m ()
deleteTokenByUuid uuid = do
  _ <- findUserTokenByUuid uuid
  deleteUserTokenByUuid uuid
  return ()

deleteTokenByUserUuid
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
  -> m ()
deleteTokenByUserUuid userUuid = do
  userTokens <- findUserTokensByUserUuid userUuid
  traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens

cleanTokens
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
  => m ()
cleanTokens = do
  deletedUserTokens <- deleteUserTokensWithExpiration
  logInfoI _CMP_SERVICE $ f' "Deleted the following %s tokens" [show deletedUserTokens]
