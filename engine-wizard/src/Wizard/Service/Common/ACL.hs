module Wizard.Service.Common.ACL
  ( checkPermission
  , module Wizard.Constant.ACL
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, unless)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Constant.ACL
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

checkPermission :: String -> AppContextM ()
checkPermission perm = do
  mCurrentUser <- asks _appContextCurrentUser
  case mCurrentUser of
    Nothing -> throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER
    Just user ->
      unless
        (perm `elem` (user ^. permissions))
        (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ perm))
