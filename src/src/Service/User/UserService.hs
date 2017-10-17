module Service.User.UserService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.UUID

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
  let user = fromDTO userCreateDto uuid roles
  insertUser context user
  return $ toDTO user

getUserById :: Context -> String -> IO (Maybe UserDTO)
getUserById context userUuid = do
  maybeUser <- findUserById context userUuid
  case maybeUser of
    Just user -> return . Just $ toDTO user
    Nothing -> return Nothing
  

-- modifyUser :: Context -> String -> IO (Maybe UserDTO)
-- modifyUser context userUuid = do
--   maybeUser <- findUserById context userUuid
--   case maybeUser of
--     Just user -> return . Just $ toDTO user
--     Nothing -> return Nothing
        

-- deleteUser :: Context -> String -> IO (Maybe UserDTO)
-- deleteUser context userUuid = do
--   maybeUser <- findUserById context userUuid
--   case maybeUser of
--     Just user -> return . Just $ toDTO user
--     Nothing -> return Nothing
    