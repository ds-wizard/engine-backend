module Wizard.Service.User.UserService where

import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Messaging.Out.Topic.User
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Common
import Wizard.Service.Mail.Mailer
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserValidation

getUsers :: AppContextM [UserDTO]
getUsers = do
  users <- findUsers
  return . fmap toDTO $ users

createUserByAdmin :: UserCreateDTO -> AppContextM UserDTO
createUserByAdmin reqDto = do
  uUuid <- liftIO generateUuid
  createUserByAdminWithUuid reqDto uUuid

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> AppContextM UserDTO
createUserByAdminWithUuid reqDto uUuid = do
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  appConfig <- asks _appContextApplicationConfig
  let uRole = fromMaybe (appConfig ^. roles . defaultRole) (reqDto ^. role)
  let uPermissions = getPermissionForRole appConfig uRole
  createUser reqDto uUuid uPasswordHash uRole uPermissions

registrateUser :: UserCreateDTO -> AppContextM UserDTO
registrateUser reqDto = do
  checkIfRegistrationIsEnabled
  uUuid <- liftIO generateUuid
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  appConfig <- asks _appContextApplicationConfig
  let uRole = appConfig ^. roles . defaultRole
  let uPermissions = getPermissionForRole appConfig uRole
  createUser reqDto uUuid uPasswordHash uRole uPermissions

createUser :: UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> AppContextM UserDTO
createUser reqDto uUuid uPasswordHash uRole uPermissions = do
  validateUserEmailUniqueness (reqDto ^. email)
  now <- liftIO getCurrentTime
  let user = fromUserCreateDTO reqDto uUuid uPasswordHash uRole uPermissions now now
  insertUser user
  actionKey <- createActionKey uUuid RegistrationActionKey
  publishToUserCreatedTopic user
  catchError
    (sendRegistrationConfirmationMail (toDTO user) (actionKey ^. hash))
    (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
  sendAnalyticsEmailIfEnabled user
  return $ toDTO user
  where
    sendAnalyticsEmailIfEnabled user = do
      appConfig <- asks _appContextApplicationConfig
      when (appConfig ^. analytics . enabled) (sendRegistrationCreatedAnalyticsMail (toDTO user))

getUserById :: String -> AppContextM UserDTO
getUserById userUuid = do
  user <- findUserById userUuid
  return $ toDTO user

modifyUser :: String -> UserChangeDTO -> AppContextM UserDTO
modifyUser userUuid reqDto = do
  user <- findUserById userUuid
  validateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email)
  appConfig <- asks _appContextApplicationConfig
  updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions appConfig reqDto user)
  updateUserById updatedUser
  return . toDTO $ updatedUser
  where
    getPermissions appConfig reqDto oldUser =
      if (reqDto ^. role) /= (oldUser ^. role)
        then getPermissionForRole appConfig (reqDto ^. role)
        else oldUser ^. permissions

modifyProfile :: String -> UserProfileChangeDTO -> AppContextM UserDTO
modifyProfile userUuid reqDto = do
  user <- findUserById userUuid
  validateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email)
  updatedUser <- updateUserTimestamp $ fromUserProfileChangeDTO reqDto user
  updateUserById updatedUser
  return . toDTO $ updatedUser

changeUserPasswordByAdmin :: String -> UserPasswordDTO -> AppContextM ()
changeUserPasswordByAdmin userUuid reqDto = do
  user <- findUserById userUuid
  passwordHash <- generatePasswordHash (reqDto ^. password)
  now <- liftIO getCurrentTime
  updateUserPasswordById userUuid passwordHash now
  return ()

changeCurrentUserPassword :: String -> UserPasswordDTO -> AppContextM ()
changeCurrentUserPassword userUuid reqDto = do
  user <- findUserById userUuid
  passwordHash <- generatePasswordHash (reqDto ^. password)
  now <- liftIO getCurrentTime
  updateUserPasswordById userUuid passwordHash now
  return ()

changeUserPasswordByHash :: String -> Maybe String -> UserPasswordDTO -> AppContextM ()
changeUserPasswordByHash userUuid maybeHash userPasswordDto = do
  akHash <- validateHash maybeHash
  user <- findUserById userUuid
  actionKey <- getActionKeyByHash akHash
  passwordHash <- generatePasswordHash (userPasswordDto ^. password)
  now <- liftIO getCurrentTime
  updateUserPasswordById userUuid passwordHash now
  deleteActionKey (actionKey ^. hash)
  return ()
  where
    validateHash maybeHash =
      case maybeHash of
        Just akHash -> return akHash
        Nothing -> throwError . UserError $ _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS

resetUserPassword :: ActionKeyDTO -> AppContextM ()
resetUserPassword reqDto = do
  user <- findUserByEmail (reqDto ^. email)
  actionKey <- createActionKey (user ^. uuid) ForgottenPasswordActionKey
  catchError
    (sendResetPasswordMail (toDTO user) (actionKey ^. hash))
    (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT)

changeUserState :: String -> Maybe String -> UserStateDTO -> AppContextM UserStateDTO
changeUserState userUuid maybeHash userStateDto = do
  akHash <- validateHash maybeHash
  user <- findUserById userUuid
  actionKey <- getActionKeyByHash akHash
  updatedUser <- updateUserTimestamp $ user & active .~ (userStateDto ^. active)
  updateUserById updatedUser
  deleteActionKey (actionKey ^. hash)
  return userStateDto
  where
    validateHash maybeHash =
      case maybeHash of
        Just akHash -> return akHash
        Nothing -> throwError $ UserError _ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS

deleteUser :: String -> AppContextM ()
deleteUser userUuid = do
  _ <- findUserById userUuid
  deleteUserById userUuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPermissionForRole :: AppConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. roles . admin
    "DATASTEWARD" -> config ^. roles . dataSteward
    "RESEARCHER" -> config ^. roles . researcher
    _ -> []

generatePasswordHash :: String -> AppContextM String
generatePasswordHash password = liftIO $ BS.unpack <$> makePassword (BS.pack password) 17

updateUserTimestamp :: User -> AppContextM User
updateUserTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user & updatedAt ?~ now

checkIfRegistrationIsEnabled = checkIfFeatureIsEnabled "Registration" (general . registrationEnabled)
