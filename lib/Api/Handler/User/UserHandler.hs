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
import Model.Config.DSWConfig
import Common.Error
import Service.User.UserService

getUsersA :: Context -> DSWConfig -> Scotty.ActionM ()
getUsersA context dswConfig =
  checkPermission context "UM_PERM" $ do
    eitherDtos <- liftIO $ getUsers context
    case eitherDtos of
      Right dtos -> sendJson dtos
      Left error -> sendError error

postUsersA :: Context -> DSWConfig -> Scotty.ActionM ()
postUsersA context dswConfig =
  getReqDto $ \reqDto ->
    isAdmin context $ \isAdmin -> do
      eitherUserDto <- liftIO $ createUser context dswConfig reqDto isAdmin
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          Scotty.status created201
          sendJson userDto

getUserCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
getUserCurrentA context dswConfig =
  getCurrentUserUuid context $ \userUuid -> do
    eitherDto <- liftIO $ getUserById context userUuid
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error

getUserA :: Context -> DSWConfig -> Scotty.ActionM ()
getUserA context dswConfig =
  checkPermission context "UM_PERM" $ do
    userUuid <- Scotty.param "userUuid"
    eitherDto <- liftIO $ getUserById context userUuid
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error

putUserCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
putUserCurrentA context dswConfig =
  getCurrentUserUuid context $ \userUuid ->
    getReqDto $ \reqDto -> do
      eitherDto <- liftIO $ modifyUser context userUuid reqDto
      case eitherDto of
        Right dto -> sendJson dto
        Left error -> sendError error

putUserA :: Context -> DSWConfig -> Scotty.ActionM ()
putUserA context dswConfig =
  checkPermission context "UM_PERM" $
  getReqDto $ \reqDto -> do
    userUuid <- Scotty.param "userUuid"
    eitherDto <- liftIO $ modifyUser context userUuid reqDto
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error

putUserCurrentPasswordA :: Context -> DSWConfig -> Scotty.ActionM ()
putUserCurrentPasswordA context dswConfig =
  getCurrentUserUuid context $ \userUuid ->
    getReqDto $ \reqDto -> do
      maybeError <- liftIO $ changeUserPassword context userUuid Nothing reqDto True
      case maybeError of
        Nothing -> Scotty.status noContent204
        Just error -> sendError error

putUserPasswordA :: Context -> DSWConfig -> Scotty.ActionM ()
putUserPasswordA context dswConfig =
  getReqDto $ \reqDto ->
    isAdmin context $ \isAdmin -> do
      userUuid <- Scotty.param "userUuid"
      hash <- getQueryParam "hash"
      maybeError <- liftIO $ changeUserPassword context userUuid (liftM T.unpack hash) reqDto isAdmin
      case maybeError of
        Nothing -> Scotty.status noContent204
        Just error -> sendError error

changeUserStateA :: Context -> DSWConfig -> Scotty.ActionM ()
changeUserStateA context dswConfig =
  getReqDto $ \reqDto -> do
    userUuid <- Scotty.param "userUuid"
    hash <- getQueryParam "hash"
    maybeError <- liftIO $ changeUserState context userUuid (liftM T.unpack hash) reqDto
    case maybeError of
      Nothing -> sendJson reqDto
      Just error -> sendError error

deleteUserA :: Context -> DSWConfig -> Scotty.ActionM ()
deleteUserA context dswConfig =
  checkPermission context "UM_PERM" $ do
    userUuid <- Scotty.param "userUuid"
    maybeError <- liftIO $ deleteUser context userUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
