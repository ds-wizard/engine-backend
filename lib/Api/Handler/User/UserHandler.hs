module Api.Handler.User.UserHandler where

import Control.Monad.Reader (liftM)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.User.UserChangeJM ()
import Api.Resource.User.UserCreateJM ()
import Api.Resource.User.UserJM ()
import Api.Resource.User.UserPasswordJM ()
import Api.Resource.User.UserProfileChangeJM ()
import Api.Resource.User.UserStateJM ()
import Service.User.UserService

getUsersA :: Endpoint
getUsersA =
  checkPermission "UM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDtos <- runInAuthService getUsers
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postUsersA :: Endpoint
postUsersA =
  getReqDto $ \reqDto ->
    isAdmin $ \isAdmin -> do
      eitherUserDto <- runInUnauthService $ createUser reqDto isAdmin
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
  getCurrentUserUuid $ \userUuid ->
    getAuthServiceExecutor $ \runInAuthService -> do
      eitherDto <- runInAuthService $ getUserById userUuid
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

getUserA :: Endpoint
getUserA =
  checkPermission "UM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    userUuid <- param "userUuid"
    eitherDto <- runInAuthService $ getUserById userUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

putUserCurrentA :: Endpoint
putUserCurrentA =
  getCurrentUserUuid $ \userUuid ->
    getReqDto $ \reqDto ->
      getAuthServiceExecutor $ \runInAuthService -> do
        eitherDto <- runInAuthService $ modifyProfile userUuid reqDto
        case eitherDto of
          Right dto -> json dto
          Left error -> sendError error

putUserA :: Endpoint
putUserA =
  checkPermission "UM_PERM" $
  getReqDto $ \reqDto ->
    getAuthServiceExecutor $ \runInAuthService -> do
      userUuid <- param "userUuid"
      eitherDto <- runInAuthService $ modifyUser userUuid reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error

putUserCurrentPasswordA :: Endpoint
putUserCurrentPasswordA =
  getCurrentUserUuid $ \userUuid ->
    getReqDto $ \reqDto ->
      getAuthServiceExecutor $ \runInAuthService -> do
        maybeError <- runInAuthService $ changeCurrentUserPassword userUuid reqDto
        case maybeError of
          Nothing -> status noContent204
          Just error -> sendError error

putUserPasswordA :: Endpoint
putUserPasswordA =
  getReqDto $ \reqDto ->
    isAdmin $ \isAdmin -> do
      userUuid <- param "userUuid"
      if isAdmin
        then getAuthServiceExecutor $ \runInAuthService -> do
               maybeError <- runInAuthService $ changeUserPasswordByAdmin userUuid reqDto
               case maybeError of
                 Nothing -> status noContent204
                 Just error -> sendError error
        else do
          hash <- getQueryParam "hash"
          maybeError <- runInUnauthService $ changeUserPasswordByHash userUuid (liftM T.unpack hash) reqDto
          case maybeError of
            Nothing -> status noContent204
            Just error -> sendError error

changeUserStateA :: Endpoint
changeUserStateA =
  getReqDto $ \reqDto -> do
    userUuid <- param "userUuid"
    hash <- getQueryParam "hash"
    if isNothing hash
      then getAuthServiceExecutor $ \runInAuthService -> do
             maybeError <- runInAuthService $ changeUserState userUuid (liftM T.unpack hash) reqDto
             case maybeError of
               Nothing -> json reqDto
               Just error -> sendError error
      else do
        maybeError <- runInUnauthService $ changeUserState userUuid (liftM T.unpack hash) reqDto
        case maybeError of
          Nothing -> json reqDto
          Just error -> sendError error

deleteUserA :: Endpoint
deleteUserA =
  checkPermission "UM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    userUuid <- param "userUuid"
    maybeError <- runInAuthService $ deleteUser userUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
