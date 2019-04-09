module Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.User.UserChangeDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserProfileChangeDTO
import Api.Resource.User.UserStateDTO
import Database.DAO.User.UserDAO
import LensesConfig
import Localization
import Messaging.Out.Topic.UserTopic
import Model.ActionKey.ActionKey
import Model.Config.AppConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.User.User
import Service.ActionKey.ActionKeyService
import Service.Mail.Mailer
import Service.User.UserMapper
import Service.User.UserValidation
import Util.Uuid

getUsers :: AppContextM (Either AppError [UserDTO])
getUsers = heFindUsers $ \users -> return . Right $ toDTO <$> users

createUserByAdmin :: UserCreateDTO -> AppContextM (Either AppError UserDTO)
createUserByAdmin reqDto = do
  uUuid <- liftIO generateUuid
  createUserByAdminWithUuid reqDto uUuid

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> AppContextM (Either AppError UserDTO)
createUserByAdminWithUuid reqDto uUuid = do
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  dswConfig <- asks _appContextConfig
  let uRole = fromMaybe (dswConfig ^. roles . defaultRole) (reqDto ^. role)
  let uPermissions = getPermissionForRole dswConfig uRole
  createUser reqDto uUuid uPasswordHash uRole uPermissions

registrateUser :: UserCreateDTO -> AppContextM (Either AppError UserDTO)
registrateUser reqDto = do
  uUuid <- liftIO generateUuid
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  dswConfig <- asks _appContextConfig
  let uRole = dswConfig ^. roles . defaultRole
  let uPermissions = getPermissionForRole dswConfig uRole
  createUser reqDto uUuid uPasswordHash uRole uPermissions

createUser :: UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> AppContextM (Either AppError UserDTO)
createUser reqDto uUuid uPasswordHash uRole uPermissions =
  heValidateUserEmailUniqueness (reqDto ^. email) $ do
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash uRole uPermissions now now
    insertUser user
    heCreateActionKey uUuid RegistrationActionKey $ \actionKey -> do
      publishToUserCreatedTopic user
      emailResult <- sendRegistrationConfirmationMail (toDTO user) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Left $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT
        _ -> do
          sendAnalyticsEmailIfEnabled user
          return . Right $ toDTO user
  where
    sendAnalyticsEmailIfEnabled user = do
      dswConfig <- asks _appContextConfig
      if dswConfig ^. analytics . enabled
        then sendRegistrationCreatedAnalyticsMail (toDTO user)
        else return $ Right ()

getUserById :: String -> AppContextM (Either AppError UserDTO)
getUserById userUuid = heFindUserById userUuid $ \user -> return . Right $ toDTO user

modifyUser :: String -> UserChangeDTO -> AppContextM (Either AppError UserDTO)
modifyUser userUuid reqDto =
  heFindUserById userUuid $ \user ->
    heValidateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email) $ do
      dswConfig <- asks _appContextConfig
      updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions dswConfig reqDto user)
      updateUserById updatedUser
      return . Right . toDTO $ updatedUser
  where
    getPermissions dswConfig reqDto oldUser =
      if (reqDto ^. role) /= (oldUser ^. role)
        then getPermissionForRole dswConfig (reqDto ^. role)
        else oldUser ^. permissions

modifyProfile :: String -> UserProfileChangeDTO -> AppContextM (Either AppError UserDTO)
modifyProfile userUuid reqDto =
  heFindUserById userUuid $ \user ->
    heValidateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email) $ do
      updatedUser <- updateUserTimestamp $ fromUserProfileChangeDTO reqDto user
      updateUserById updatedUser
      return . Right . toDTO $ updatedUser

changeUserPasswordByAdmin :: String -> UserPasswordDTO -> AppContextM (Maybe AppError)
changeUserPasswordByAdmin userUuid reqDto =
  hmFindUserById userUuid $ \user -> do
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    return Nothing

changeCurrentUserPassword :: String -> UserPasswordDTO -> AppContextM (Maybe AppError)
changeCurrentUserPassword userUuid reqDto =
  hmFindUserById userUuid $ \user -> do
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    return Nothing

changeUserPasswordByHash :: String -> Maybe String -> UserPasswordDTO -> AppContextM (Maybe AppError)
changeUserPasswordByHash userUuid maybeHash userPasswordDto =
  validateHash maybeHash $ \akHash ->
    hmFindUserById userUuid $ \user ->
      hmGetActionKeyByHash akHash $ \actionKey -> do
        passwordHash <- generatePasswordHash (userPasswordDto ^. password)
        now <- liftIO getCurrentTime
        updateUserPasswordById userUuid passwordHash now
        deleteActionKey (actionKey ^. hash)
        return Nothing
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing ->
          return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS

resetUserPassword :: ActionKeyDTO -> AppContextM (Maybe AppError)
resetUserPassword reqDto =
  hmFindUserByEmail (reqDto ^. email) $ \user ->
    hmCreateActionKey (user ^. uuid) ForgottenPasswordActionKey $ \actionKey -> do
      emailResult <- sendResetPasswordMail (toDTO user) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Just $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT
        _ -> return Nothing

createForgottenUserPassword :: String -> UserPasswordDTO -> AppContextM (Maybe AppError)
createForgottenUserPassword userUuid userPasswordDto =
  hmFindUserById userUuid $ \user -> do
    passwordHash <- generatePasswordHash (userPasswordDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    return Nothing

changeUserState :: String -> Maybe String -> UserStateDTO -> AppContextM (Maybe AppError)
changeUserState userUuid maybeHash userStateDto =
  validateHash maybeHash $ \akHash ->
    hmFindUserById userUuid $ \user ->
      hmGetActionKeyByHash akHash $ \actionKey -> do
        updatedUser <- updateUserTimestamp $ user & active .~ (userStateDto ^. active)
        updateUserById updatedUser
        deleteActionKey (actionKey ^. hash)
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing -> return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS

deleteUser :: String -> AppContextM (Maybe AppError)
deleteUser userUuid =
  hmFindUserById userUuid $ \user -> do
    deleteUserById userUuid
    return Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPermissionForRole :: AppConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. roles ^. admin
    "DATASTEWARD" -> config ^. roles ^. dataSteward
    "RESEARCHER" -> config ^. roles ^. researcher
    _ -> []

generatePasswordHash :: String -> AppContextM String
generatePasswordHash password = liftIO $ BS.unpack <$> makePassword (BS.pack password) 17

updateUserTimestamp :: User -> AppContextM User
updateUserTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user & updatedAt .~ Just now
