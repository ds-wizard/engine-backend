module Registry.Model.Context.AppContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import RegistryLib.Model.Organization.Organization
import RegistryLib.Model.Organization.OrganizationRole
import Shared.Common.Model.Error.Error

getCurrentOrganization :: AppContextM Organization
getCurrentOrganization = do
  mCurrentOrganization <- asks currentOrganization
  case mCurrentOrganization of
    Just org -> return org
    Nothing -> throwError $ ForbiddenError _ERROR_MODEL_APP_CONTEXT__MISSING_ORGANIZATION

isOrganizationAdmin :: AppContextM Bool
isOrganizationAdmin = do
  mOrg <- asks currentOrganization
  case mOrg of
    Just org -> return $ org.oRole == AdminRole
    Nothing -> return False
