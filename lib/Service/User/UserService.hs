module Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.UUID as U

import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserStateDTO
import Common.Context
import Common.DSWConfig
import Common.Error
import Common.Types
import Common.Uuid
import Database.DAO.User.UserDAO
import Model.ActionKey.ActionKey
import Model.User.User
import Service.ActionKey.ActionKeyService
import Service.Mail.Mailer
import Service.Token.TokenService
import Service.User.UserMapper

getPermissionForRole :: DSWConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. dswcfgRoles ^. acrAdmin
    "DATASTEWARD" -> config ^. dswcfgRoles ^. acrDataSteward
    "RESEARCHER" -> config ^. dswcfgRoles ^. acrResearcher
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
    then return . Left . createErrorWithFieldError $ ("email", "User with given email is already exists")
    else do
      passwordHash <- makePassword (BS.pack (userCreateDto ^. ucdtoPassword)) 17
      buildUser config userCreateDto userUuid (BS.unpack passwordHash) (userCreateDto ^. ucdtoRole) isAdmin $ \user -> do
        insertUser context user
        createActionKey context userUuid RegistrationActionKey
        return . Right $ toDTO user
  where
    buildUser config dto userUuid password maybeRole isAdmin callback =
      callback $ fromUserCreateDTO dto userUuid password role permissions False
      where
        permissions = getPermissionForRole config role
        role =
          case maybeRole of
            Just r ->
              if isAdmin
                then r
                else defaultRole
            Nothing -> defaultRole
        defaultRole = config ^. dswcfgRoles . acrDefaultRole

--      sendRegistrationConfirmationMail (config ^. dswcfgMail) (user ^. uEmail)
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
        then return . Left . createErrorWithFieldError $ ("email", "User with given email is already exists")
        else do
          let updatedUser = fromUserDTO userDto (user ^. uUuid) (user ^. uPasswordHash) (user ^. uIsActive)
          updateUserById context updatedUser
          return . Right $ userDto
    Left error -> return . Left $ error
  where
    isAlreadyUsedAndIsNotMine (Right user) = U.toString (user ^. uUuid) /= userUuid
    isAlreadyUsedAndIsNotMine (Left _) = False

changeUserPassword :: Context -> String -> UserPasswordDTO -> IO (Maybe AppError)
changeUserPassword context userUuid userPasswordDto = do
  eitherUser <- findUserById context userUuid
  passwordHash <- makePassword (BS.pack (userPasswordDto ^. updtoPassword)) 17
  case eitherUser of
    Right user -> do
      updateUserPasswordById context userUuid (BS.unpack passwordHash)
      return Nothing
    Left error -> return . Just $ error

changeUserState :: Context -> String -> Maybe String -> UserStateDTO -> IO (Maybe AppError)
changeUserState context userUuid maybeHash userStateDto =
  validateHash maybeHash $ \hash -> do
    eitherUser <- findUserById context userUuid
    case eitherUser of
      Right user -> do
        eitherActionKey <- getActionKeyByHash context hash
        case eitherActionKey of
          Right actionKey -> do
            let updatedUser = user & uIsActive .~ (userStateDto ^. usdtoActive)
            updateUserById context updatedUser
            deleteActionKey context (actionKey ^. akHash)
          Left error -> return . Just $ error
      Left error -> return . Just $ error
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just hash -> callback hash
        Nothing -> return . Just . createErrorWithErrorMessage $ "Hash query param has to be provided"

deleteUser :: Context -> String -> IO (Maybe AppError)
deleteUser context userUuid = do
  eitherUser <- findUserById context userUuid
  case eitherUser of
    Right user -> do
      deleteUserById context userUuid
      return Nothing
    Left error -> return . Just $ error
