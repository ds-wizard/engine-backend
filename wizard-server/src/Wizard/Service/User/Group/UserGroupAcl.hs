module Wizard.Service.User.Group.UserGroupAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Model.User.UserGroup

checkViewPermission :: HasField "uuid" user U.UUID => UserGroup -> [user] -> AppContextM ()
checkViewPermission userGroup users = do
  currentUser <- getCurrentUser
  let userUuids = fmap (.uuid) users
  hasPermission <- hasPermission _USERS_MANAGE_ROLE_PERMISSION
  unless
    (hasPermission || not userGroup.private || currentUser.uuid `elem` userUuids)
    (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View UserGroup")
