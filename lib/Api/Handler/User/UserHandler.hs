module Api.Handler.User.UserHandler where

import Control.Monad.Reader (lift, liftM)
import qualified Data.Text as T
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Service.User.UserService

getUsersA :: Endpoint
getUsersA =
  checkPermission "UM_PERM" $ do
    eitherDtos <- lift getUsers
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postUsersA :: Endpoint
postUsersA =
  getReqDto $ \reqDto ->
    isAdmin $ \isAdmin -> do
      eitherUserDto <- lift $ createUser reqDto isAdmin
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          status created201
          json userDto
  where
    createUser reqDto isAdmin =
      if isAdmin
        then createUserByAdmin reqDto
        else registrateUser reqDto

getUserCurrentA :: Endpoint
getUserCurrentA =
  getCurrentUserUuid $ \userUuid -> do
    eitherDto <- lift $ getUserById userUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

getUserA :: Endpoint
getUserA =
  checkPermission "UM_PERM" $ do
    userUuid <- param "userUuid"
    eitherDto <- lift $ getUserById userUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putUserCurrentA :: Endpoint
putUserCurrentA =
  getCurrentUserUuid $ \userUuid ->
    getReqDto $ \reqDto -> do
      eitherDto <- lift $ modifyProfile userUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

putUserA :: Endpoint
putUserA =
  checkPermission "UM_PERM" $
  getReqDto $ \reqDto -> do
    userUuid <- param "userUuid"
    eitherDto <- lift $ modifyUser userUuid reqDto
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putUserCurrentPasswordA :: Endpoint
putUserCurrentPasswordA =
  getCurrentUserUuid $ \userUuid ->
    getReqDto $ \reqDto -> do
      maybeError <- lift $ changeCurrentUserPassword userUuid reqDto
      case maybeError of
        Nothing -> status noContent204
        Just error -> sendError error

putUserPasswordA :: Endpoint
putUserPasswordA =
  getReqDto $ \reqDto ->
    isAdmin $ \isAdmin -> do
      userUuid <- param "userUuid"
      hash <- getQueryParam "hash"
      maybeError <- lift $ changeUserPassword userUuid (liftM T.unpack hash) reqDto isAdmin
      case maybeError of
        Nothing -> status noContent204
        Just error -> sendError error
  where
    changeUserPassword userUuid hash reqDto isAdmin =
      if isAdmin
        then changeUserPasswordByAdmin userUuid reqDto
        else changeUserPasswordByHash userUuid hash reqDto

changeUserStateA :: Endpoint
changeUserStateA =
  getReqDto $ \reqDto -> do
    userUuid <- param "userUuid"
    hash <- getQueryParam "hash"
    maybeError <- lift $ changeUserState userUuid (liftM T.unpack hash) reqDto
    case maybeError of
      Nothing -> json reqDto
      Just error -> sendError error

deleteUserA :: Endpoint
deleteUserA =
  checkPermission "UM_PERM" $ do
    userUuid <- param "userUuid"
    maybeError <- lift $ deleteUser userUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
