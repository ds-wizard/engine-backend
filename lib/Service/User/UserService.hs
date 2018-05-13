module Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
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
import Common.Context
import Common.Error
import Common.Localization
import Common.Types
import Common.Uuid
import Database.DAO.User.UserDAO
import LensesConfig
import Model.ActionKey.ActionKey
import Model.Config.DSWConfig
import Model.User.User
import Service.ActionKey.ActionKeyService
import Service.Mail.Mailer
import Service.User.UserMapper
import Service.User.UserValidation

getUsers :: Context -> IO (Either AppError [UserDTO])
getUsers context = heFindUsers context $ \users -> return . Right $ toDTO <$> users

createUserByAdmin :: Context -> DSWConfig -> UserCreateDTO -> IO (Either AppError UserDTO)
createUserByAdmin context config reqDto = do
  uUuid <- generateUuid
  createUserByAdminWithUuid context config reqDto uUuid

createUserByAdminWithUuid :: Context -> DSWConfig -> UserCreateDTO -> U.UUID -> IO (Either AppError UserDTO)
createUserByAdminWithUuid context config reqDto uUuid = do
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  let uRole = fromMaybe (config ^. roles ^. defaultRole) (reqDto ^. role)
  let uPermissions = getPermissionForRole config uRole
  createUser context config reqDto uUuid uPasswordHash uRole uPermissions

registrateUser :: Context -> DSWConfig -> UserCreateDTO -> IO (Either AppError UserDTO)
registrateUser context config reqDto = do
  uUuid <- generateUuid
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  let uRole = config ^. roles ^. defaultRole
  let uPermissions = getPermissionForRole config uRole
  createUser context config reqDto uUuid uPasswordHash uRole uPermissions

createUser ::
     Context -> DSWConfig -> UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> IO (Either AppError UserDTO)
createUser context config reqDto uUuid uPasswordHash uRole uPermissions =
  heValidateUserEmailUniqueness context (reqDto ^. email) $ do
    now <- getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash uRole uPermissions now now
    insertUser context user
    heCreateActionKey context uUuid RegistrationActionKey $ \actionKey -> do
      sendRegistrationConfirmationMail config (user ^. email) (actionKey ^. userId) (actionKey ^. hash)
      return . Right $ toDTO user

getUserById :: Context -> String -> IO (Either AppError UserDTO)
getUserById context userUuid = heFindUserById context userUuid $ \user -> return . Right $ toDTO user

modifyUser :: Context -> DSWConfig -> String -> UserChangeDTO -> IO (Either AppError UserDTO)
modifyUser context dswConfig userUuid reqDto =
  heFindUserById context userUuid $ \user ->
    heValidateUserChangedEmailUniqueness context (reqDto ^. email) (user ^. email) $ do
      updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions reqDto user)
      updateUserById context updatedUser
      return . Right . toDTO $ updatedUser
  where
    getPermissions reqDto oldUser =
      if (reqDto ^. role) /= (oldUser ^. role)
        then getPermissionForRole dswConfig (reqDto ^. role)
        else oldUser ^. permissions

modifyProfile :: Context -> DSWConfig -> String -> UserProfileChangeDTO -> IO (Either AppError UserDTO)
modifyProfile context dswConfig userUuid reqDto =
  heFindUserById context userUuid $ \user ->
    heValidateUserChangedEmailUniqueness context (reqDto ^. email) (user ^. email) $ do
      updatedUser <- updateUserTimestamp $ fromUserProfileChangeDTO reqDto user
      updateUserById context updatedUser
      return . Right . toDTO $ updatedUser

changeUserPasswordByAdmin :: Context -> String -> UserPasswordDTO -> IO (Maybe AppError)
changeUserPasswordByAdmin context userUuid reqDto =
  hmFindUserById context userUuid $ \user -> do
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- getCurrentTime
    updateUserPasswordById context userUuid passwordHash now
    return Nothing

changeCurrentUserPassword :: Context -> String -> UserPasswordDTO -> IO (Maybe AppError)
changeCurrentUserPassword context userUuid reqDto =
  hmFindUserById context userUuid $ \user -> do
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- getCurrentTime
    updateUserPasswordById context userUuid passwordHash now
    return Nothing

changeUserPasswordByHash :: Context -> String -> Maybe String -> UserPasswordDTO -> IO (Maybe AppError)
changeUserPasswordByHash context userUuid maybeHash userPasswordDto =
  validateHash maybeHash $ \akHash ->
    hmFindUserById context userUuid $ \user ->
      hmGetActionKeyByHash context akHash $ \actionKey -> do
        passwordHash <- generatePasswordHash (userPasswordDto ^. password)
        now <- getCurrentTime
        updateUserPasswordById context userUuid passwordHash now
        deleteActionKey context (actionKey ^. hash)
        return Nothing
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing ->
          return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS

resetUserPassword :: Context -> DSWConfig -> ActionKeyDTO -> IO (Maybe AppError)
resetUserPassword context config reqDto =
  hmFindUserByEmail context (reqDto ^. email) $ \user ->
    hmCreateActionKey context (user ^. uuid) ForgottenPasswordActionKey $ \actionKey -> do
      sendResetPasswordMail config (user ^. email) (actionKey ^. userId) (actionKey ^. hash)
      return Nothing

createForgottenUserPassword :: Context -> String -> UserPasswordDTO -> IO (Maybe AppError)
createForgottenUserPassword context userUuid userPasswordDto =
  hmFindUserById context userUuid $ \user -> do
    passwordHash <- generatePasswordHash (userPasswordDto ^. password)
    now <- getCurrentTime
    updateUserPasswordById context userUuid passwordHash now
    return Nothing

changeUserState :: Context -> String -> Maybe String -> UserStateDTO -> IO (Maybe AppError)
changeUserState context userUuid maybeHash userStateDto =
  validateHash maybeHash $ \akHash ->
    hmFindUserById context userUuid $ \user ->
      hmGetActionKeyByHash context akHash $ \actionKey -> do
        updatedUser <- updateUserTimestamp $ user & isActive .~ (userStateDto ^. active)
        updateUserById context updatedUser
        deleteActionKey context (actionKey ^. hash)
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing -> return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS

deleteUser :: Context -> String -> IO (Maybe AppError)
deleteUser context userUuid =
  hmFindUserById context userUuid $ \user -> do
    deleteUserById context userUuid
    return Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPermissionForRole :: DSWConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. roles ^. admin
    "DATASTEWARD" -> config ^. roles ^. dataSteward
    "RESEARCHER" -> config ^. roles ^. researcher
    _ -> []

generatePasswordHash :: String -> IO String
generatePasswordHash password = BS.unpack <$> makePassword (BS.pack password) 17

updateUserTimestamp :: User -> IO User
updateUserTimestamp user = do
  now <- getCurrentTime
  return $ user & updatedAt .~ Just now
