module Service.User.UserService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.UUID as U

import Api.Resources.User.UserCreateDTO
import Api.Resources.User.UserDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.UserDAO
import Database.Entity.User
import Service.User.UserMapper

getPermissionForRole :: Role -> [Permission]
getPermissionForRole _ = ["ADD_CHAPTER", "EDIT_CHAPTER", "DELETE_CHAPTER"]

getUsers :: Context -> IO [UserDTO]
getUsers context = do
  users <- findUsers context
  return . fmap toDTO $ users

createUser :: Context -> UserCreateDTO -> IO UserDTO
createUser context userCreateDto = do
  uuid <- generateUuid
  let roles = getPermissionForRole (userCreateDto ^. ucdtoRole)
  let user = fromUserCreateDTO userCreateDto uuid roles
  insertUser context user
  return $ toDTO user

getUserById :: Context -> String -> IO (Maybe UserDTO)
getUserById context userUuid = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> return . Just $ toDTO user
    Nothing -> return Nothing

modifyUser :: Context -> String -> UserDTO -> IO (Maybe UserDTO)
modifyUser context userUuid userDto = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> do
      let user = fromUserDTO userDto
      updateUserById context user
      return . Just $ userDto
    Nothing -> return Nothing

deleteUser :: Context -> String -> IO Bool
deleteUser context userUuid = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> do
      deleteUserById context userUuid
      return True
    Nothing -> return False
