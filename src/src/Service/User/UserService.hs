module Service.User.UserService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.UUID as U

import Api.Resources.User.UserCreateDTO
import Api.Resources.User.UserDTO
import Api.Resources.User.UserPasswordDTO
import Common.Error
import Common.Types
import Common.Uuid
import Context
import DSPConfig
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

getUsers :: Context -> IO [UserDTO]
getUsers context = do
  users <- findUsers context
  return . fmap toDTO $ users

createUser :: Context
           -> DSPConfig
           -> UserCreateDTO
           -> IO (Either AppError UserDTO)
createUser context config userCreateDto = do
  uuid <- generateUuid
  createUserWithGivenUuid context config uuid userCreateDto

createUserWithGivenUuid :: Context
                        -> DSPConfig
                        -> U.UUID
                        -> UserCreateDTO
                        -> IO (Either AppError UserDTO)
createUserWithGivenUuid context config userUuid userCreateDto = do
  userFromDb <- findUserByEmail context (userCreateDto ^. ucdtoEmail)
  if isJust userFromDb
    then return . Left . createErrorWithFormError $
         "User with given email is already exists"
    else do
      let roles = getPermissionForRole config (userCreateDto ^. ucdtoRole)
      passwordHash <- makePassword (BS.pack (userCreateDto ^. ucdtoPassword)) 17
      let user =
            fromUserCreateDTO
              userCreateDto
              userUuid
              (BS.unpack passwordHash)
              roles
      insertUser context user
      return . Right $ toDTO user

getCurrentUser :: Context -> Maybe T.Text -> IO (Maybe UserDTO)
getCurrentUser context tokenHeader = do
  let userUuidMaybe = getUserUuidFromToken context tokenHeader :: Maybe T.Text
  case userUuidMaybe of
    Just userUuid -> getUserById context (T.unpack userUuid)
    _ -> return Nothing

getUserById :: Context -> String -> IO (Maybe UserDTO)
getUserById context userUuid = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> return . Just $ toDTO user
    Nothing -> return Nothing

modifyCurrentUser :: Context -> Maybe T.Text -> UserDTO -> IO (Maybe UserDTO)
modifyCurrentUser context tokenHeader userDto = do
  let userUuidMaybe = getUserUuidFromToken context tokenHeader :: Maybe T.Text
  case userUuidMaybe of
    Just userUuid -> modifyUser context (T.unpack userUuid) userDto
    _ -> return Nothing

modifyUser :: Context -> String -> UserDTO -> IO (Maybe UserDTO)
modifyUser context userUuid userDto = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> do
      let updatedUser =
            fromUserDTO userDto (user ^. uUuid) (user ^. uPasswordHash)
      updateUserById context updatedUser
      return . Just $ userDto
    Nothing -> return Nothing

changeCurrentUserPassword :: Context
                          -> Maybe T.Text
                          -> UserPasswordDTO
                          -> IO Bool
changeCurrentUserPassword context tokenHeader userPasswordDto = do
  let userUuidMaybe = getUserUuidFromToken context tokenHeader :: Maybe T.Text
  case userUuidMaybe of
    Just userUuid ->
      changeUserPassword context (T.unpack userUuid) userPasswordDto
    _ -> return False

changeUserPassword :: Context -> String -> UserPasswordDTO -> IO Bool
changeUserPassword context userUuid userPasswordDto = do
  maybeUser <- findUserById context userUuid
  passwordHash <- makePassword (BS.pack (userPasswordDto ^. updtoPassword)) 17
  case maybeUser of
    Just user -> do
      updateUserPasswordById context userUuid (BS.unpack passwordHash)
      return True
    Nothing -> return False

deleteUser :: Context -> String -> IO Bool
deleteUser context userUuid = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> do
      deleteUserById context userUuid
      return True
    Nothing -> return False
