module Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Either
import qualified Data.UUID as U

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
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

getPermissionForRole :: DSWConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. roles ^. admin
    "DATASTEWARD" -> config ^. roles ^. dataSteward
    "RESEARCHER" -> config ^. roles ^. researcher
    _ -> []

getUsers :: Context -> IO (Either AppError [UserDTO])
getUsers context = do
  eitherUsers <- findUsers context
  case eitherUsers of
    Right users -> return . Right . fmap toDTO $ users
    Left error -> return . Left $ error

createUser :: Context -> DSWConfig -> UserCreateDTO -> Bool -> IO (Either AppError UserDTO)
createUser context config userCreateDto isAdmin = do
  uuid <- generateUuid
  createUserWithGivenUuid context config uuid userCreateDto isAdmin

createUserWithGivenUuid :: Context -> DSWConfig -> U.UUID -> UserCreateDTO -> Bool -> IO (Either AppError UserDTO)
createUserWithGivenUuid context config userUuid userCreateDto isAdmin = do
  eitherUserFromDb <- findUserByEmail context (userCreateDto ^. ucdtoEmail)
  if isRight eitherUserFromDb
    then return . Left . createErrorWithFieldError $
         ("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ userCreateDto ^. ucdtoEmail)
    else do
      passwordHash <- makePassword (BS.pack (userCreateDto ^. ucdtoPassword)) 17
      buildUser config userCreateDto userUuid (BS.unpack passwordHash) (userCreateDto ^. ucdtoRole) isAdmin $ \user -> do
        insertUser context user
        eitherActionKey <- createActionKey context userUuid RegistrationActionKey
        case eitherActionKey of
          Right actionKey -> do
            sendRegistrationConfirmationMail config (user ^. uEmail) (actionKey ^. userId) (actionKey ^. hash)
            return . Right $ toDTO user
          Left error -> return . Left $ error
  where
    buildUser config dto userUuid password maybeRole isAdmin callback =
      callback $ fromUserCreateDTO dto userUuid password userRole permissions False
      where
        permissions = getPermissionForRole config userRole
        userRole =
          case maybeRole of
            Just r ->
              if isAdmin
                then r
                else appDefaultRole
            Nothing -> appDefaultRole
        appDefaultRole = config ^. roles ^. defaultRole

getUserById :: Context -> String -> IO (Either AppError UserDTO)
getUserById context userUuid = do
  eitherUser <- findUserById context userUuid
  case eitherUser of
    Right user -> return . Right $ toDTO user
    Left error -> return . Left $ error

modifyUser :: Context -> String -> UserDTO -> IO (Either AppError UserDTO)
modifyUser context userUuid userDto = do
  eitherUser <- findUserById context userUuid
  case eitherUser of
    Right user -> do
      eitherUserFromDb <- findUserByEmail context (userDto ^. udtoEmail)
      if isAlreadyUsedAndIsNotMine eitherUserFromDb
        then return . Left . createErrorWithFieldError $
             ("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ userDto ^. udtoEmail)
        else do
          let updatedUser = fromUserDTO userDto (user ^. uUuid) (user ^. uPasswordHash) (user ^. uIsActive)
          updateUserById context updatedUser
          return . Right $ userDto
    Left error -> return . Left $ error
  where
    isAlreadyUsedAndIsNotMine (Right user) = U.toString (user ^. uUuid) /= userUuid
    isAlreadyUsedAndIsNotMine (Left _) = False

changeUserPassword :: Context -> String -> Maybe String -> UserPasswordDTO -> Bool -> IO (Maybe AppError)
changeUserPassword context userUuid maybeHash userPasswordDto isAdminOrCurrentUser = do
  eitherUser <- findUserById context userUuid
  case eitherUser of
    Right user ->
      isAllowedToChangePassword $ do
        passwordHash <- makePassword (BS.pack (userPasswordDto ^. updtoPassword)) 17
        updateUserPasswordById context userUuid (BS.unpack passwordHash)
        return Nothing
    Left error -> return . Just $ error
  where
    isAllowedToChangePassword callback =
      if isAdminOrCurrentUser
        then callback
        else case maybeHash of
               Just akHash -> do
                 eitherActionKey <- getActionKeyByHash context akHash
                 case eitherActionKey of
                   Right actionKey -> do
                     deleteActionKey context (actionKey ^. hash)
                     callback
                   Left error -> return . Just $ error
               Nothing ->
                 return . Just $
                 createErrorWithErrorMessage _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS

resetUserPassword :: Context -> DSWConfig -> ActionKeyDTO -> IO (Maybe AppError)
resetUserPassword context config reqDto = do
  eitherUser <- findUserByEmail context (reqDto ^. email)
  case eitherUser of
    Right user -> do
      eitherActionKey <- createActionKey context (user ^. uUuid) ForgottenPasswordActionKey
      case eitherActionKey of
        Right actionKey -> do
          sendResetPasswordMail config (user ^. uEmail) (actionKey ^. userId) (actionKey ^. hash)
          return Nothing
        Left error -> return . Just $ error
    Left error -> return . Just $ error

createForgottenUserPassword :: Context -> String -> UserPasswordDTO -> IO (Maybe AppError)
createForgottenUserPassword context userUuid userPasswordDto = do
  eitherUser <- findUserById context userUuid
  passwordHash <- makePassword (BS.pack (userPasswordDto ^. updtoPassword)) 17
  case eitherUser of
    Right user -> do
      updateUserPasswordById context userUuid (BS.unpack passwordHash)
      return Nothing
    Left error -> return . Just $ error

changeUserState :: Context -> String -> Maybe String -> UserStateDTO -> IO (Maybe AppError)
changeUserState context userUuid maybeHash userStateDto =
  validateHash maybeHash $ \akHash -> do
    eitherUser <- findUserById context userUuid
    case eitherUser of
      Right user -> do
        eitherActionKey <- getActionKeyByHash context akHash
        case eitherActionKey of
          Right actionKey -> do
            let updatedUser = user & uIsActive .~ (userStateDto ^. usdtoActive)
            updateUserById context updatedUser
            deleteActionKey context (actionKey ^. hash)
          Left error -> return . Just $ error
      Left error -> return . Just $ error
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing -> return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS

deleteUser :: Context -> String -> IO (Maybe AppError)
deleteUser context userUuid = do
  eitherUser <- findUserById context userUuid
  case eitherUser of
    Right user -> do
      deleteUserById context userUuid
      return Nothing
    Left error -> return . Just $ error
