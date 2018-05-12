module Api.Handler.User.UserHandler where

import Control.Monad.Reader (asks, liftIO, liftM)
import Control.Monad.Trans.Class (lift)
import Data.Text as T
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Model.Context.AppContext
import Service.User.UserService

getUsersA :: Endpoint
getUsersA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "UM_PERM" $ do
    eitherDtos <- liftIO $ getUsers context
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postUsersA :: Endpoint
postUsersA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getReqDto $ \reqDto ->
    isAdmin context $ \isAdmin -> do
      eitherUserDto <- liftIO $ createUser context dswConfig reqDto isAdmin
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          status created201
          json userDto
  where
    createUser context dswConfig reqDto isAdmin =
      if isAdmin
        then createUserByAdmin context dswConfig reqDto
        else registrateUser context dswConfig reqDto

getUserCurrentA :: Endpoint
getUserCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getCurrentUserUuid context $ \userUuid -> do
    eitherDto <- liftIO $ getUserById context userUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

getUserA :: Endpoint
getUserA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "UM_PERM" $ do
    userUuid <- param "userUuid"
    eitherDto <- liftIO $ getUserById context userUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putUserCurrentA :: Endpoint
putUserCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getCurrentUserUuid context $ \userUuid ->
    getReqDto $ \reqDto -> do
      eitherDto <- liftIO $ modifyProfile context dswConfig userUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

putUserA :: Endpoint
putUserA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "UM_PERM" $
    getReqDto $ \reqDto -> do
      userUuid <- param "userUuid"
      eitherDto <- liftIO $ modifyUser context dswConfig userUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

putUserCurrentPasswordA :: Endpoint
putUserCurrentPasswordA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getCurrentUserUuid context $ \userUuid ->
    getReqDto $ \reqDto -> do
      maybeError <- liftIO $ changeCurrentUserPassword context userUuid reqDto
      case maybeError of
        Nothing -> status noContent204
        Just error -> sendError error

putUserPasswordA :: Endpoint
putUserPasswordA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getReqDto $ \reqDto ->
    isAdmin context $ \isAdmin -> do
      userUuid <- param "userUuid"
      hash <- getQueryParam "hash"
      maybeError <- liftIO $ changeUserPassword context userUuid (liftM T.unpack hash) reqDto isAdmin
      case maybeError of
        Nothing -> status noContent204
        Just error -> sendError error
  where
    changeUserPassword context userUuid hash reqDto isAdmin =
      if isAdmin
        then changeUserPasswordByAdmin context userUuid reqDto
        else changeUserPasswordByHash context userUuid hash reqDto

changeUserStateA :: Endpoint
changeUserStateA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getReqDto $ \reqDto -> do
    userUuid <- param "userUuid"
    hash <- getQueryParam "hash"
    maybeError <- liftIO $ changeUserState context userUuid (liftM T.unpack hash) reqDto
    case maybeError of
      Nothing -> json reqDto
      Just error -> sendError error

deleteUserA :: Endpoint
deleteUserA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "UM_PERM" $ do
    userUuid <- param "userUuid"
    maybeError <- liftIO $ deleteUser context userUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
