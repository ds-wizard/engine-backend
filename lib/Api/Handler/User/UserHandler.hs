module Api.Handler.User.UserHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.Lazy as TL
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Common.Context
import Common.DSPConfig
import Common.Error
import Service.User.UserService

getUsersA :: Context -> DSPConfig -> Scotty.ActionM ()
getUsersA context dspConfig =
  checkPermission context "UM_PERM" $ do
    eitherDtos <- liftIO $ getUsers context
    case eitherDtos of
      Right dtos -> sendJson dtos
      Left error -> sendError error

postUsersA :: Context -> DSPConfig -> Scotty.ActionM ()
postUsersA context dspConfig =
  getReqDto $ \reqDto ->
    isAdmin context $ \isAdmin -> do
      eitherUserDto <- liftIO $ createUser context dspConfig reqDto isAdmin
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          Scotty.status created201
          sendJson userDto

--      let isAdmin = False
getUserCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
getUserCurrentA context dspConfig =
  getCurrentUserUuid context $ \userUuid -> do
    eitherDto <- liftIO $ getUserById context userUuid
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error

getUserA :: Context -> DSPConfig -> Scotty.ActionM ()
getUserA context dspConfig =
  checkPermission context "UM_PERM" $ do
    userUuid <- Scotty.param "userUuid"
    eitherDto <- liftIO $ getUserById context userUuid
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error

putUserCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserCurrentA context dspConfig =
  getCurrentUserUuid context $ \userUuid ->
    getReqDto $ \reqDto -> do
      eitherDto <- liftIO $ modifyUser context userUuid reqDto
      case eitherDto of
        Right dto -> sendJson dto
        Left error -> sendError error

putUserA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserA context dspConfig =
  checkPermission context "UM_PERM" $
  getReqDto $ \reqDto -> do
    userUuid <- Scotty.param "userUuid"
    eitherDto <- liftIO $ modifyUser context userUuid reqDto
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error

putUserCurrentPasswordA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserCurrentPasswordA context dspConfig =
  getCurrentUserUuid context $ \userUuid ->
    getReqDto $ \reqDto -> do
      maybeError <- liftIO $ changeUserPassword context userUuid reqDto
      case maybeError of
        Nothing -> Scotty.status noContent204
        Just error -> sendError error

putUserPasswordA :: Context -> DSPConfig -> Scotty.ActionM ()
putUserPasswordA context dspConfig =
  checkPermission context "UM_PERM" $
  getReqDto $ \reqDto -> do
    userUuid <- Scotty.param "userUuid"
    maybeError <- liftIO $ changeUserPassword context userUuid reqDto
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error

changeUserStateA :: Context -> DSPConfig -> Scotty.ActionM ()
changeUserStateA context dspConfig =
  getReqDto $ \reqDto -> do
    userUuid <- Scotty.param "userUuid"
    hash <- getQueryParam "hash"
    maybeError <- liftIO $ changeUserState context userUuid (liftM T.unpack hash) reqDto
    case maybeError of
      Nothing -> sendJson reqDto
      Just error -> sendError error

deleteUserA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteUserA context dspConfig =
  checkPermission context "UM_PERM" $ do
    userUuid <- Scotty.param "userUuid"
    maybeError <- liftIO $ deleteUser context userUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
