module Wizard.Service.Acl.AclService (
  checkPermission,
  checkRole,
  module Wizard.Constant.Acl,
  module Wizard.Model.User.User,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, unless)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Constant.Acl
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

checkPermission :: String -> AppContextM ()
checkPermission perm = do
  mCurrentUser <- asks currentUser
  case mCurrentUser of
    Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
    Just user ->
      unless
        (perm `elem` user.permissions)
        (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ perm))

checkRole :: String -> AppContextM ()
checkRole userRole = do
  mCurrentUser <- asks currentUser
  case mCurrentUser of
    Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
    Just user ->
      unless
        (userRole == user.uRole)
        (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Required role: " ++ userRole))
