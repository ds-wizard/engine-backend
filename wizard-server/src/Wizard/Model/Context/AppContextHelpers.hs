module Wizard.Model.Context.AppContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import qualified Data.UUID as U
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.User.RoleDAO
import WizardLib.Public.Localization.Messages.Public
import qualified WizardLib.Public.Model.User.Role as Role
import WizardLib.Public.Model.User.RoleSimple

getCurrentUser :: AppContextM UserDTO
getCurrentUser = do
  mCurrentUser <- asks currentUser
  case mCurrentUser of
    Just user -> return user
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

getCurrentUserUuid :: AppContextM (Maybe U.UUID)
getCurrentUserUuid = do
  mCurrentUser <- asks currentUser
  return . fmap (.uuid) $ mCurrentUser

isCurrentUserAdmin :: AppContextM Bool
isCurrentUserAdmin = do
  mUser <- asks currentUser
  case mUser of
    Just user -> do
      role <- findRoleByUuid user.role.uuid
      return role.isAdmin
    Nothing -> return False
