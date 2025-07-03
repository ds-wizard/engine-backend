module Wizard.Service.Common where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Model.Context.AppContext

checkIfTenantFeatureIsEnabled featureName dbFunction accessor = do
  tenantConfig <- dbFunction
  if accessor tenantConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName

checkIfServerFeatureIsEnabled featureName accessor = do
  serverConfig <- asks serverConfig
  if accessor serverConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
