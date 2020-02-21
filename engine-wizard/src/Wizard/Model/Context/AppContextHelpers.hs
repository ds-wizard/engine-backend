module Wizard.Model.Context.AppContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

getCurrentUser :: AppContextM UserDTO
getCurrentUser = do
  mCurrentUser <- asks _appContextCurrentUser
  case mCurrentUser of
    Just user -> return user
    Nothing -> throwError $ UserError _ERROR_SERVICE_USER__MISSING_USER
