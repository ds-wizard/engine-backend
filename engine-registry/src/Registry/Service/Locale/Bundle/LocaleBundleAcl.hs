module Registry.Service.Locale.Bundle.LocaleBundleAcl where

import Control.Monad.Except (throwError)

import Registry.Model.Context.AppContextHelpers
import Registry.Model.Organization.Organization
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

checkWritePermission = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Write LocaleBundle"
