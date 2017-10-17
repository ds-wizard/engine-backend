module Api.Handler.User.UserHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
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

-- putUserA :: Context -> Scotty.ActionM ()
-- putUserA context = do
--   userUuid <- Scotty.param "userUuid"
--   maybeDto <- liftIO $ modifyUser context userUuid
--   case maybeDto of
--     Just dto -> Scotty.json dto
--     Nothing -> notFoundA

-- deleteUserA :: Context -> Scotty.ActionM ()
-- deleteUserA context = do
--   userUuid <- Scotty.param "userUuid"
--   maybeDto <- liftIO $ deleteUser context userUuid
--   case maybeDto of
--     Just dto -> Scotty.json dto
--     Nothing -> notFoundA
            
