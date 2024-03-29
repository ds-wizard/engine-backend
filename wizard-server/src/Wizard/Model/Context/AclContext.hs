module Wizard.Model.Context.AclContext (
  AclContext (..),
  module Wizard.Constant.Acl,
  module Wizard.Model.User.User,
) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Service.Acl.AclService
import Wizard.Api.Resource.User.UserDTO
import Wizard.Constant.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import WizardLib.Public.Localization.Messages.Public

instance AclContext AppContextM where
  checkPermission perm = do
    mCurrentUser <- asks currentUser
    case mCurrentUser of
      Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
      Just user ->
        unless
          (perm `elem` user.permissions)
          (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ perm))
