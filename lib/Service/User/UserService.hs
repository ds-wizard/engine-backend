module Service.User.UserService where

import Control.Lens ((^.))
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
import Common.Context
import Common.DSPConfig
import Common.Error
import Common.Types
import Common.Uuid
import Database.DAO.User.UserDAO
import Model.User.User
import Service.Token.TokenService
import Service.User.UserMapper

getPermissionForRole :: DSPConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. dspcfgRoles ^. acrAdmin
    "DATASTEWARD" -> config ^. dspcfgRoles ^. acrDataSteward
    "RESEARCHER" -> config ^. dspcfgRoles ^. acrResearcher
    _ -> []

getUsers :: Context -> IO (Either AppError [UserDTO])
getUsers context = do
  eitherUsers <- findUsers context
  case eitherUsers of
    Right users -> return . Right . fmap toDTO $ users
    Left error -> return . Left $ error

createUser :: Context -> DSPConfig -> UserCreateDTO -> IO (Either AppError UserDTO)
createUser context config userCreateDto = do
  uuid <- generateUuid
  createUserWithGivenUuid context config uuid userCreateDto

createUserWithGivenUuid :: Context -> DSPConfig -> U.UUID -> UserCreateDTO -> IO (Either AppError UserDTO)
createUserWithGivenUuid context config userUuid userCreateDto = do
  eitherUserFromDb <- findUserByEmail context (userCreateDto ^. ucdtoEmail)
  if isRight eitherUserFromDb
    then return . Left . createErrorWithFieldError $ ("email", "User with given email is already exists")
    else do
      let roles = getPermissionForRole config (userCreateDto ^. ucdtoRole)
      passwordHash <- makePassword (BS.pack (userCreateDto ^. ucdtoPassword)) 17
      let user = fromUserCreateDTO userCreateDto userUuid (BS.unpack passwordHash) roles
      insertUser context user
      return . Right $ toDTO user

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
          let updatedUser = fromUserDTO userDto (user ^. uUuid) (user ^. uPasswordHash)
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

deleteUser :: Context -> String -> IO (Maybe AppError)
deleteUser context userUuid = do
  eitherUser <- findUserById context userUuid
  case eitherUser of
    Right user -> do
      deleteUserById context userUuid
      return Nothing
    Left error -> return . Just $ error
