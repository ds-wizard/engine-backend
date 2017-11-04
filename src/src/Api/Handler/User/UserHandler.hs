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
import Api.Resources.User.UserPasswordDTO
import Context
import DSPConfig
import Service.User.UserService

getUsersA :: Context -> DSPConfig -> Scotty.ActionM ()
getUsersA context dspConfig = do
  dtos <- liftIO $ getUsers context
  let a = dtos :: [UserDTO]
  Scotty.json dtos

postUsersA :: Context -> DSPConfig -> Scotty.ActionM ()
postUsersA context dspConfig = do
  userCreateDto <- Scotty.jsonData
  userDto <- liftIO $ createUser context dspConfig userCreateDto
  Scotty.json userDto

getUserA :: Context -> DSPConfig -> Scotty.ActionM ()
getUserA context dspConfig = do
  userUuid <- Scotty.param "userUuid"
  maybeDto <- liftIO $ getUserById context userUuid
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA

getUserCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
getUserCurrentA context dspConfig = do
  tokenHeader <- Scotty.header "Authorization"
  maybeDto <-
    liftIO $
    getCurrentUser context (tokenHeader >>= \token -> Just . toStrict $ token)
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA

putUserA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserA context dspConfig = do
  userUuid <- Scotty.param "userUuid"
  userDto <- Scotty.jsonData
  maybeDto <- liftIO $ modifyUser context userUuid userDto
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA

putUserPasswordA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserPasswordA context dspConfig = do
  userUuid <- Scotty.param "userUuid"
  userDto <- Scotty.jsonData
  isSuccess <- liftIO $ changeUserPassword context userUuid userDto
  if isSuccess
    then Scotty.status noContent204
    else notFoundA

putUserCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserCurrentA context dspConfig = do
  tokenHeader <- Scotty.header "Authorization"
  userDto <- Scotty.jsonData :: Scotty.ActionM UserDTO
  maybeDto <-
    liftIO $
    modifyCurrentUser
      context
      (tokenHeader >>= \token -> Just . toStrict $ token)
      userDto
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA

putUserCurrentPasswordA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserCurrentPasswordA context dspConfig = do
  tokenHeader <- Scotty.header "Authorization"
  userPasswordDto <- Scotty.jsonData :: Scotty.ActionM UserPasswordDTO
  isSuccess <-
    liftIO $
    changeCurrentUserPassword
      context
      (tokenHeader >>= \token -> Just . toStrict $ token)
      userPasswordDto
  if isSuccess
    then Scotty.status noContent204
    else notFoundA

deleteUserA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteUserA context dspConfig = do
  userUuid <- Scotty.param "userUuid"
  isSuccess <- liftIO $ deleteUser context userUuid
  if isSuccess
    then Scotty.status noContent204
    else notFoundA
