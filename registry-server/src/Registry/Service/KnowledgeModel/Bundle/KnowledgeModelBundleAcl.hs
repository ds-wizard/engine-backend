module Registry.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAcl where

import Control.Monad.Except (throwError)

import Registry.Model.Context.AppContextHelpers
import RegistryLib.Model.Organization.Organization
import RegistryLib.Model.Organization.OrganizationRole
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error

checkWritePermission = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Write KnowledgeModelBundle"
