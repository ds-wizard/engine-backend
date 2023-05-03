module Wizard.Model.Context.AppContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import WizardLib.Public.Localization.Messages.Public

getCurrentUser :: AppContextM UserDTO
getCurrentUser = do
  mCurrentUser <- asks currentUser
  case mCurrentUser of
    Just user -> return user
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER
