module Api.Handler.User.UserHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.User.UserCreateDTO
import Api.Resources.User.UserDTO
import Context
import Service.User.UserService

getUsersA :: Context -> Scotty.ActionM ()
getUsersA context = do
  dtos <- liftIO $ getUsers context
  let a = dtos :: [UserDTO]
  Scotty.json dtos

postUsersA :: Context -> Scotty.ActionM ()
postUsersA context = do
  userCreateDto <- Scotty.jsonData
  userDto <- liftIO $ createUser context userCreateDto
  Scotty.json userDto

getUserA :: Context -> Scotty.ActionM ()
getUserA context = do
  userUuid <- Scotty.param "userUuid"
  maybeDto <- liftIO $ getUserById context userUuid
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA

putUserA :: Context -> Scotty.ActionM ()
putUserA context = do
  userUuid <- Scotty.param "userUuid"
  userDto <- Scotty.jsonData
  maybeDto <- liftIO $ modifyUser context userUuid userDto
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA

deleteUserA :: Context -> Scotty.ActionM ()
deleteUserA context = do
  userUuid <- Scotty.param "userUuid"
  isSuccess <- liftIO $ deleteUser context userUuid
  if isSuccess
    then Scotty.status noContent204
    else notFoundA
