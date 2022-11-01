module Wizard.Service.UserToken.UserTokenService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Token
import Shared.Util.Uuid
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenMapper
import Wizard.Service.UserToken.UserTokenValidation
import Wizard.Util.Logger

createTokenFromCredentials :: UserTokenCreateDTO -> AppContextM UserTokenDTO
createTokenFromCredentials reqDto =
  runInTransaction $ do
    mUser <- findUserByEmail' (toLower <$> reqDto ^. email)
    case mUser of
      Just user -> do
        validate reqDto user
        serverConfig <- asks _appContextServerConfig
        now <- liftIO getCurrentTime
        let updatedUser = user & lastVisitedAt .~ now
        updateUserById updatedUser
        userToken <- createToken user Nothing
        return . toDTO $ userToken
      Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

createToken :: User -> Maybe String -> AppContextM UserToken
createToken user mSessionState =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    tokenUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let userToken =
          toUserToken user tokenUuid mSessionState now (serverConfig ^. jwt) (serverConfig ^. general . secret)
    insertUserToken userToken
    return userToken

deleteTokenByUserUuid :: U.UUID -> AppContextM ()
deleteTokenByUserUuid userUuid = do
  userTokens <- findUserTokensByUserUuid userUuid
  traverse_ (\t -> deleteUserTokenById (t ^. uuid)) userTokens

deleteTokenByValue :: Maybe String -> AppContextM ()
deleteTokenByValue mTokenHeader = do
  case fmap separateToken mTokenHeader of
    Just (Just tokenValue) -> do
      userTokens <- findUserTokensByValue tokenValue
      traverse_ (\t -> deleteUserTokenById (t ^. uuid)) userTokens
    _ -> return ()

deleteTokenBySessionState :: Maybe String -> AppContextM ()
deleteTokenBySessionState mSessionState = do
  case mSessionState of
    Just sessionState -> do
      userTokens <- findUserTokensBySessionState sessionState
      traverse_ (\t -> deleteUserTokenById (t ^. uuid)) userTokens
    Nothing -> return ()

cleanTokens :: AppContextM ()
cleanTokens = do
  serverConfig <- asks _appContextServerConfig
  deletedUserTokens <- deleteUserTokensWithExpiration (serverConfig ^. jwt . expiration)
  logInfoU _CMP_SERVICE $ f' "Deleted the following %s tokens" [show deletedUserTokens]
