module Registry.Model.Context.AppContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Registry.Model.Organization.Organization
import Shared.Model.Error.Error

getCurrentOrganization :: AppContextM Organization
getCurrentOrganization = do
  mCurrentOrganization <- asks currentOrganization
  case mCurrentOrganization of
    Just org -> return org
    Nothing -> throwError $ ForbiddenError _ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION
