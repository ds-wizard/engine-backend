module Wizard.Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
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
import Wizard.Messaging.Out.Topic.UserTopic
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Common
import Wizard.Service.Mail.Mailer
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserValidation

getUsers :: AppContextM (Either AppError [UserDTO])
getUsers = heFindUsers $ \users -> return . Right $ toDTO <$> users

createUserByAdmin :: UserCreateDTO -> AppContextM (Either AppError UserDTO)
createUserByAdmin reqDto = do
  uUuid <- liftIO generateUuid
  createUserByAdminWithUuid reqDto uUuid

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> AppContextM (Either AppError UserDTO)
createUserByAdminWithUuid reqDto uUuid = do
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  appConfig <- asks _appContextApplicationConfig
  let uRole = fromMaybe (appConfig ^. roles . defaultRole) (reqDto ^. role)
  let uPermissions = getPermissionForRole appConfig uRole
  createUser reqDto uUuid uPasswordHash uRole uPermissions

registrateUser :: UserCreateDTO -> AppContextM (Either AppError UserDTO)
registrateUser reqDto =
  heCheckIfRegistrationIsEnabled $ do
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash (reqDto ^. password)
    appConfig <- asks _appContextApplicationConfig
    let uRole = appConfig ^. roles . defaultRole
    let uPermissions = getPermissionForRole appConfig uRole
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
      appConfig <- asks _appContextApplicationConfig
      if appConfig ^. analytics . enabled
        then sendRegistrationCreatedAnalyticsMail (toDTO user)
        else return $ Right ()

getUserById :: String -> AppContextM (Either AppError UserDTO)
getUserById userUuid = heFindUserById userUuid $ \user -> return . Right $ toDTO user

modifyUser :: String -> UserChangeDTO -> AppContextM (Either AppError UserDTO)
modifyUser userUuid reqDto =
  heFindUserById userUuid $ \user ->
    heValidateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email) $ do
      appConfig <- asks _appContextApplicationConfig
      updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions appConfig reqDto user)
      updateUserById updatedUser
      return . Right . toDTO $ updatedUser
  where
    getPermissions appConfig reqDto oldUser =
      if (reqDto ^. role) /= (oldUser ^. role)
        then getPermissionForRole appConfig (reqDto ^. role)
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
        Nothing -> return . Just . UserError $ _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS

resetUserPassword :: ActionKeyDTO -> AppContextM (Maybe AppError)
resetUserPassword reqDto =
  hmFindUserByEmail (reqDto ^. email) $ \user ->
    hmCreateActionKey (user ^. uuid) ForgottenPasswordActionKey $ \actionKey -> do
      emailResult <- sendResetPasswordMail (toDTO user) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Just $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT
        _ -> return Nothing

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
        Nothing -> return . Just . UserError $ _ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS

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

heCheckIfRegistrationIsEnabled = heCheckIfFeatureIsEnabled "Registration" (general . registrationEnabled)
